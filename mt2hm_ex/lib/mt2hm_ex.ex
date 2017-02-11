defmodule Mt2hmEx do
  def main([from, to]) do
    process(from, to)
  end

  def process(from_file, to_file) do
    from_file
    |> File.stream!
    |> skip_until_header
    |> ignore_footer_lines
    |> fix_broken_lines
    |> decode_csv
    |> reject_aborted_transactions
    |> convert_records
    |> encode_csv
    |> dump_output(to_file)
  end

  defp skip_until_header(stream) do
    Stream.drop_while(stream, &not_header_yet?/1)
  end

  defp ignore_footer_lines(stream) do
    Stream.reject(stream, &footer?/1)
  end

  defp footer?("\"  Типы операций\"\n"), do: true
  defp footer?("\"T - сумма обработана\"\n"), do: true
  defp footer?("\"A - сумма блокирована (ожидает обработки)\"\n"), do: true
  defp footer?("\"E - ошибка выполнения операции\"\n"), do: true
  defp footer?("\"Исходящий остаток:\"" <> _), do: true
  defp footer?(_), do: false

  defp fix_broken_lines(stream) do
    Stream.map(stream, &fix_broken_line/1)
  end

  defp not_header_yet?("\"Тип\"" <> _), do: false
  defp not_header_yet?(_), do: true

  defp fix_broken_line(line) do
    import String, only: [replace: 3]

    line
    |> replace(~r/^"/, "BEGBEGBEG")
    |> replace(~r/"(?=[\r\n])/m, "ENDENDEND")
    |> replace(~r/","/, "MIDMIDMID")
    |> replace(~r/"/, "\"\"")
    |> replace(~r/BEGBEGBEG|ENDENDEND/, "\"")
    |> replace("MIDMIDMID", "\",\"")
  end

  defp decode_csv(stream) do
    CSV.decode(stream, headers: true)
  end

  defp convert_records(stream) do
    Stream.map(stream, &convert_record/1)
  end

  defp output_headers do
    [:date, :account, :category, :total, :currency, :description, :transfer]
  end

  defp convert_record(record) do
    Enum.into(output_headers, %{}, &{&1, csv_value(&1, record)})
  end

  defp csv_value(:date, %{"Дата операции" => val}),
    do: String.slice(val, 0, 10)
  defp csv_value(:account, %{"Карта" => "417753******4576"}),
    do: "ЗП МТБанк"
  defp csv_value(:account, %{"Карта" => "417753******8022"}),
    do: "МТБанк USD"
  defp csv_value(:account, %{"Карта" => _}),
    do: "ХЗ"
  defp csv_value(:category, _), do: "Без категории"
  defp csv_value(:total, %{"Сумма в валюте счета" => val}),
    do: String.replace(val, ", ", "")
  defp csv_value(:currency, %{"Карта" => "417753******4576"}),
    do: "BYN"
  defp csv_value(:currency, %{"Карта" => "417753******8022"}),
    do: "USD"
  defp csv_value(:currency, %{"Карта" => _}),
    do: "ХЗ"
  defp csv_value(:transfer, _), do: nil
  defp csv_value(
      :description,
      %{"Место операции" => place,
        "Oписание операции" => description,
        "Валюта" => currency,
        "Сумма в валюте операции" => amount
      }
    ) do
      "Место: #{place}, " <>
        "Описание: #{description}, " <>
        "Сумма: #{amount} #{currency}"
  end

  defp reject_aborted_transactions(stream) do
    Stream.reject(stream, &aborted_transaction?/1)
  end

  defp aborted_transaction?(%{"Тип" => "E"}), do: true
  defp aborted_transaction?(%{"Тип" => _}), do: false

  defp encode_csv(stream) do
    CSV.encode(stream, headers: output_headers, separator: ?;)
  end

  defp dump_output(stream, to_file) do
    file = File.stream!(to_file)
    Enum.into(stream, file)
  end
end
