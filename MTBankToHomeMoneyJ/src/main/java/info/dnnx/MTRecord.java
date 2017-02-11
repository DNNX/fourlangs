package info.dnnx;

public class MTRecord {
    //# ["Тип",
    //#  "Дата операции",
    //#  "Дата обработки",
    //#  "Место операции",
    //#  "Oписание операции",
    //#  "Карта",
    //#  "Валюта",
    //#  "Сумма в валюте операции",
    //#  "Сумма в валюте счета",
    //#  "Остаток счета"]
    private String date;
    private String place;
    private String description;
    private String currency;
    private String amountInOperationCurrency;
    private String amountInAccountCurrency;

    public MTRecord(String date, String place, String description, String currency, String amountInOperationCurrency, String amountInAccountCurrency) {
        this.date = date;
        this.place = place;
        this.description = description;
        this.currency = currency;
        this.amountInOperationCurrency = amountInOperationCurrency;
        this.amountInAccountCurrency = amountInAccountCurrency;
    }

    public String getDate() {
        return date;
    }

    public String getPlace() {
        return place;
    }

    public String getDescription() {
        return description;
    }

    public String getCurrency() {
        return currency;
    }

    public String getAmountInOperationCurrency() {
        return amountInOperationCurrency;
    }

    public String getAmountInAccountCurrency() {
        return amountInAccountCurrency;
    }
}
