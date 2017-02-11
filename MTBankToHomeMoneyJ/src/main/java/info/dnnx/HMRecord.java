package info.dnnx;

import java.util.Arrays;
import java.util.List;

public class HMRecord {
    public static List<String> CsvHeader = Arrays.asList("date", "account", "category", "total", "currency", "description", "transfer");

    private final String date;
    private final String account;
    private final String category;
    private final String total;
    private final String currency;
    private final String description;
    private final String transfer;

    public HMRecord(String date, String account, String category, String total, String currency, String description, String transfer) {
        this.date = date;
        this.account = account;
        this.category = category;
        this.total = total;
        this.currency = currency;
        this.description = description;
        this.transfer = transfer;
    }

    public List<String> csvValues() {
        return Arrays.asList(date, account, category, total, currency, description, transfer);
    }
}
