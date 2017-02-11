package info.dnnx;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.CSVRecord;

import java.io.*;
import java.nio.charset.Charset;
import java.util.ArrayList;

public class Main {
    public static void main(String[] args) throws IOException {
        final String fromFile = "/Users/dnnx/Downloads/3014_20170108_155616.csv"; //args[0];
        final String toFile = "lol.csv"; // args[1];
        convert(fromFile, toFile);
    }

    private static void convert(final String fromFile, final String toFile) throws IOException {
        File tempFile = null;
        try {
            tempFile = File.createTempFile("fixed", ".csv");
            fixMtbankCsv(fromFile, tempFile);
            final Iterable<MTRecord> mtRecords = readFixedMTBankCSV(tempFile);
            final Iterable<HMRecord> hmRecords = convertRecords(mtRecords);
            dumpHomemoneyCsv(toFile, hmRecords);
        } finally {
            if (tempFile != null) {
                tempFile.delete();
            }
        }
    }

    private static void fixMtbankCsv(String fromFile, File toFile) throws IOException {
        try (BufferedReader br = new BufferedReader(new FileReader(fromFile))) {
            try (BufferedWriter bw = new BufferedWriter(new FileWriter(toFile))) {
                String line = null;
                boolean ignoring = true;
                while (true) {
                    line = br.readLine();
                    if (line == null) {
                        break;
                    }
                    if (ignoring) {
                        if (line.startsWith("\"Тип\"")) {
                            ignoring = false;
                        } else {
                            continue;
                        }
                    }
                    final String fixedLine = fixLine(line);
                    bw.write(fixedLine);
                    bw.newLine();
                }
            }
        }
    }

    private static String fixLine(String line) {
        return line
                .replaceAll("^\"", "BEGBEGBEG")
                .replaceAll("\"$", "ENDENDEND")
                .replaceAll("\",\"", "MIDMIDMID")
                .replaceAll("\"", "\"\"")
                .replaceAll("BEGBEGBEG|ENDENDEND", "\"")
                .replaceAll("MIDMIDMID", "\",\"");
    }

    private static Iterable<MTRecord> readFixedMTBankCSV(File fromFile) throws IOException {
        CSVParser parser = CSVParser.parse(fromFile, Charset.defaultCharset(), CSVFormat.RFC4180.withFirstRecordAsHeader());
        ArrayList<MTRecord> result = new ArrayList<MTRecord>();

        for (CSVRecord record : parser) {
            result.add(makeMTRecord(record));
        }

        return result;
    }

    private static MTRecord makeMTRecord(CSVRecord record) {
        return
                new MTRecord(
                        record.get("Дата операции"),
                        record.get("Место операции"),
                        record.get("Oписание операции"),
                        record.get("Валюта"),
                        record.get("Сумма в валюте операции"),
                        record.get("Сумма в валюте счета")
                );
    }

    private static Iterable<HMRecord> convertRecords(Iterable<MTRecord> mtRecords) {
        ArrayList<HMRecord> result = new ArrayList<HMRecord>();

        for (MTRecord mtRecord : mtRecords) {
            result.add(convertRecord(mtRecord));
        }

        return result;
    }

    private static HMRecord convertRecord(MTRecord mtRecord) {
        return new HMRecord(
                mtRecord.getDate().substring(0, 10),
                "МТБанк USD",
                "Без категории",
                mtRecord.getAmountInAccountCurrency().replaceAll(", ", ""),
                "USD",
                hmDescription(mtRecord),
                "");
    }

    private static String hmDescription(MTRecord mtRecord) {
        return "Место: " +
                mtRecord.getPlace() +
                ", Описание: " +
                mtRecord.getDescription() +
                ", Сумма: " +
                mtRecord.getAmountInOperationCurrency() +
                ' ' +
                mtRecord.getCurrency();
    }

    private static void dumpHomemoneyCsv(String toFile, Iterable<HMRecord> hmRecords) throws IOException {
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(toFile))) {
            CSVPrinter printer = new CSVPrinter(bw, CSVFormat.RFC4180.withDelimiter(';'));
            printer.printRecord(HMRecord.CsvHeader);
            for (HMRecord record : hmRecords) {
                printer.printRecord(record.csvValues());
            }
        }
    }
}
