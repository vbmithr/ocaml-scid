cstruct intraday_header {
  uint8_t header[4];
  uint32_t header_size;
  uint32_t record_size;
  uint16_t version;
  uint16_t unused1;
  uint32_t utcstartindex;
  uint8_t reserve[36]
} as little_endian

cstruct intraday_record {
  uint64_t datetime;
  uint32_t o;
  uint32_t h;
  uint32_t l;
  uint32_t c;
  uint32_t num_trades;
  uint32_t total_volume;
  uint32_t bid_volume;
  uint32_t ask_volume
} as little_endian
