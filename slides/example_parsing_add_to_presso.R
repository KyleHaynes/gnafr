parse_examples <- c(
  "LOT 1 20/110 musgrave rd red hill qld 4059",
  "20 LOT 110 musgrave rd red hill qld 4059",
  "20/110 musgrave rd red hill qld 4059",
  "UNIT 20/110 musgrave rd red hill qld 4059",
  "U20 110 mus grave rd red hill qld 4059",
  "U20 110 musgrave ride red hill qld 4059",
  "U20/110 musgrave rd, red hill qld 4059",
  "BUILDING NAME 20/110-120 musgrave rd red hill qld 4059",
  "BUILDING NAME 20 110-120 musgrave rd red hill qld 4059",
  "XXXXXX 123 232 20/110-120 musgrave rd red hill qld 4059",
  "XXXXXX 123 232 UNIT 20/110-120 musgrave rd red hill qld 4059",
  "1 flash unit in UNIT 20/110-120 musgrave rd red hill qld 4059"  
)

parsed <- address_parse(parse_examples)
parsed[]
parsed[, .(in_flat_number, in_level_number, in_lot_number,
           in_number_first, in_street_name, in_locality)]


con <- gnaf_connect("C:/temp/gnafx23.duckdb")
 gnaf_match(parse_examples, con)
