package main

// FIXME: remove explicit gunzip in favor of accept-encoding: https://github.com/aws/aws-sdk-go/issues/1292

import (
	"bytes"
	"compress/gzip"
	"encoding/csv"
	"encoding/json"
	"fmt"
	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/aws/aws-sdk-go/service/s3"
	"github.com/aws/aws-sdk-go/service/s3/s3manager"
	"github.com/dgryski/go-onlinestats"
	log "github.com/sirupsen/logrus"
	"gopkg.in/cheggaaa/pb.v1"
	"io"
	"math"
	"os"
	"path"
	"regexp"
	"strconv"
	"strings"
	"sync"
	"sync/atomic"
)

// worker that reads CSV rows read from files downloaded and gunzip'd from S3 keys
func csv_reader(downloader *s3manager.Downloader, bucket string, file_count *uint32,
	keys <-chan string, rows chan<- []string) {
	for key := range keys {
		buff := &aws.WriteAtBuffer{}

		_, err := downloader.Download(buff,
			&s3.GetObjectInput{
				Bucket: aws.String(bucket),
				Key:    aws.String(key),
			})
		if err != nil {
			log.Error("unable to download", key, err)
			return
		}

		gzbuff := bytes.NewReader(buff.Bytes())
		zr, err := gzip.NewReader(gzbuff)
		if err != nil {
			log.Error("unable to uncompress", key, err)
			return
		}
		defer zr.Close()
		csvr := csv.NewReader(zr)
		for {
			row, err := csvr.Read()
			if err == io.EOF {
				break
			}
			if err != nil {
				log.Error("unable to read CSV row", key, err)
				return
			}
			rows <- row
		}

		atomic.AddUint32(file_count, 1)
	}
}

type TotalStat struct {
	total float64
	stats onlinestats.Running
}

type Summary struct {
	size    TotalStat
	etag    map[string]*TotalStat
	storage map[string]*TotalStat
}

func summarizer(bar *pb.ProgressBar, file_count *uint32,
	cols []string, rows <-chan []string, summary chan<- *Summary) {

	last_file_count := uint32(0)

	colmap := make(map[string]int, len(cols))
	for i, col := range cols {
		colmap[col] = i
	}

	report := new(Summary)
	report.size.total = 0.0
	report.etag = make(map[string]*TotalStat, 1000)
	report.storage = make(map[string]*TotalStat, 3)
	for row := range rows {
		cur_file_count := atomic.LoadUint32(file_count)
		if cur_file_count != last_file_count {
			last_file_count = cur_file_count
			bar.Postfix(fmt.Sprintf(" [completed %d files]", last_file_count))
		}
		bar.Increment()

		size, err := strconv.ParseFloat(row[colmap["Size"]], 64)
		if err != nil {
			log.Error("unable to convert size", err)
			continue
		}
		storage_class := row[colmap["StorageClass"]]
		etag := row[colmap["ETag"]]

		report.size.total += size
		report.size.stats.Push(size)

		if report.etag[etag] == nil {
			report.etag[etag] = new(TotalStat)
		}
		report.etag[etag].total += size
		report.etag[etag].stats.Push(size)

		if report.storage[storage_class] == nil {
			report.storage[storage_class] = new(TotalStat)
		}
		report.storage[storage_class].total += size
		report.storage[storage_class].stats.Push(size)
	}
	summary <- report
}

func sizeof_fmt(num float64, suffix string) string {
	for _, unit := range []string{"", "Ki", "Mi", "Gi", "Ti", "Pi", "Ei", "Zi"} {
		if math.Abs(num) < 1024.0 {
			return fmt.Sprintf("%3.1f%s%s", num, unit, suffix)
		}
		num /= 1024.0
	}
	return fmt.Sprintf("%.1f%s%s", num, "Yi", suffix)
}

func main() {
	log.SetFormatter(&log.TextFormatter{FullTimestamp: true})
	log.SetLevel(log.DebugLevel)

	if len(os.Args) != 3 {
		fmt.Fprintf(os.Stderr, "usage: %s <s3_inventory_manifest_path> <concurrency>\n",
			path.Base(os.Args[0]))
		os.Exit(1)
	}

	items := strings.SplitN(os.Args[1], "/", 2)
	bucket_name := items[0]
	manifest_path := items[1]
	concurrency, err := strconv.Atoi(os.Args[2])
	if err != nil {
		log.Fatal("invalid concurrency: ", err)
	}

	sess := session.Must(session.NewSessionWithOptions(session.Options{
		SharedConfigState: session.SharedConfigEnable,
	}))
	downloader := s3manager.NewDownloader(sess)

	buff := &aws.WriteAtBuffer{}

	_, err = downloader.Download(buff,
		&s3.GetObjectInput{
			Bucket: aws.String(bucket_name),
			Key:    aws.String(manifest_path),
		})
	if err != nil {
		log.Fatal("unable to download manifest: ", err)
	}

	var manifest_obj interface{}
	err = json.Unmarshal(buff.Bytes(), &manifest_obj)
	if err != nil {
		log.Fatal("failed to parse manifest: ", err)
	}

	manifest := manifest_obj.(map[string]interface{})
	files := manifest["files"].([]interface{})
	log.Infof("starting summary of %d inventory files for %s...",
		len(files), manifest["version"])

	if concurrency > len(files) {
		concurrency = len(files)
	}

	var wg sync.WaitGroup
	wg.Add(concurrency)

	keys := make(chan string, 100)
	rows := make(chan []string, 1000)
	file_count := uint32(0)

	for w := 1; w <= concurrency; w++ {
		go func() {
			defer wg.Done()
			csv_reader(downloader, bucket_name, &file_count, keys, rows)
		}()
	}

	bar := pb.New(0)
	bar.Prefix("rows processed:")
	bar.ShowBar = false
	bar.ShowCounters = true
	bar.ShowSpeed = true
	bar.Start()

	schema := manifest["fileSchema"].(string)
	cols := regexp.MustCompile(`\s*,\s*`).Split(schema, -1)
	summary := make(chan *Summary)
	go summarizer(bar, &file_count, cols, rows, summary)

	for _, fileref_obj := range files {
		fileref := fileref_obj.(map[string]interface{})
		keys <- fileref["key"].(string)
	}
	close(keys)

	wg.Wait()
	close(rows)

	report := <-summary
	bar.Finish()

	log.Infof("total: count=%d using=%s bytes=%.1f mean=%.1f stddev=%.1f",
		report.size.stats.Len(), sizeof_fmt(report.size.total, "B"), report.size.total,
		report.size.stats.Mean(), report.size.stats.Stddev())

	if len(report.storage) > 1 {
		// FIXME: need a per class TOTAL! sheesh and for csv etags, too!
		for class, storage := range report.storage {
			log.Infof("%s sizes: count=%d using=%s bytes=%.1f mean=%.1f stddev=%.1f",
				class, storage.stats.Len(),
				sizeof_fmt(storage.total, "B"), storage.total,
				storage.stats.Mean(), storage.stats.Stddev())
		}
	}

	etag_fn := fmt.Sprintf("etags-%s.csv", manifest["version"])
	log.Infof("processing %d ETags...", len(report.etag))
	csv_file, err := os.Create(etag_fn)
	if err != nil {
		log.Fatalf("unable to open %s, %v", etag_fn, err)
	}
	csv_writer := csv.NewWriter(csv_file)
	csv_writer.Write([]string{"etag", "count", "bytes", "mean", "stddev"})

	bar = pb.New(len(report.etag))
	bar.Prefix("etags processed:")
	bar.ShowBar = false
	bar.ShowCounters = true
	bar.ShowSpeed = true
	bar.Start()

	for etag, total := range report.etag {
		record := [5]string{}
		record[0] = etag
		record[1] = fmt.Sprintf("%d", total.stats.Len())
		record[2] = fmt.Sprintf("%f", total.total)
		record[3] = fmt.Sprintf("%f", total.stats.Mean())
		record[4] = fmt.Sprintf("%f", total.stats.Stddev())
		err := csv_writer.Write(record[:])
		if err != nil {
			log.Fatal("unable to write etag stats: ", err)
		}
		bar.Increment()
	}
	bar.Finish()

	log.Info("saved: ", etag_fn)
}
