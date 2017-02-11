require 'stringio'
require 'csv'

def process(from, to)
  File.open(from, 'r') do |fin|
    mtbank = parse_mtbank(fin)
    homemoney = convert(mtbank)
    File.open(to, 'w') do |fout|
      dump_homemoney(fout, homemoney)
    end
  end
end

def parse_mtbank(file)
  fixed_mtbank_csv = fix_mtbank_csv(file)
  File.write('tmp.csv', fixed_mtbank_csv.read.to_s)
  parse_mtbank_csv('tmp.csv')
end

def dump_homemoney(file, data)
  CSV.open(file, 'w', col_sep: ';') do |csv|
    csv << hm_keys
    data.each do |row|
      csv << row.values_at(*hm_keys)
    end
  end
end

def convert(mtbank)
  mtbank.map do |row|
    convert_row(row.to_h)
  end
end

# ["Тип",
#  "Дата операции",
#  "Дата обработки",
#  "Место операции",
#  "Oписание операции",
#  "Карта",
#  "Валюта",
#  "Сумма в валюте операции",
#  "Сумма в валюте счета",
#  "Остаток счета"]
#
# date;account;category;total;currency;description;transfer
def convert_row(attributes)
  hm_keys.map do |key|
    [key, __send__("hm_#{key}", attributes)]
  end.to_h
end

def hm_keys
  %i[date account category total currency description transfer]
end

def hm_date(attributes)
  attributes.fetch('Дата операции')[0...10]
end

def hm_account(_)
  'МТБанк USD'
end

def hm_category(_)
  'Без категории'
end

def hm_total(attributes)
  attributes.fetch('Сумма в валюте счета').gsub(/, /, '')
end

def hm_currency(_)
  'USD'
end

def hm_transfer(_)
  nil
end

def hm_description(attributes)
  "Место: #{attributes.fetch('Место операции')}, " \
    "Описание: #{attributes.fetch('Oписание операции')}, " \
    "Сумма: #{attributes.fetch('Сумма в валюте операции')} #{attributes.fetch('Валюта')}"
end

def fix_mtbank_csv(file)
  buf = String.new
  header = true
  file.each_line do |line|
    if header
      if line.start_with?('"Тип","Дата операции"')
        buf << line
        header = false
      end
    else
      buf << line
        .gsub(/^"/, 'BEGBEGBEG')
        .gsub(/"(?=[\r\n])/m, 'ENDENDEND')
        .gsub('","', 'MIDMIDMID')
        .gsub('"', '""')
        .gsub(/BEGBEGBEG|ENDENDEND/, '"')
        .gsub('MIDMIDMID', '","')
    end
  end
  StringIO.new(buf)
end

def parse_mtbank_csv(file)
  CSV.read(file, col_sep: ',', headers: true)
end

# puts parse_mtbank_csv(StringIO.new(a)).map(&:to_h)
fname = '/Users/dnnx/Downloads/3014_20170108_155616.csv'
# puts convert(parse_mtbank(File.open(fname)))
process(fname, 'out.csv')
