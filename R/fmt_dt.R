#' Format a datetime with a formatting string
#'
#' @param input The input datetime value. The appropriate representation should
#'   use the following construction outlined in the ISO 8601:2004 standard:
#'   `YYYY-MM-DDThh:mm:ss.s<TZD>` although some allowances made here to ease
#'   this restrictiveness (for example, the literal `T` separating the date and
#'   time components is optional). Seconds, fractional seconds, and the
#'   time-zone designation are all optional.
#' @param format The datetime formatting string.
#' @param use_tz We can optionally override any time zone information in the
#'   datetime input with a time-zone designation provided here.
#' @param locale The output locale to use for formatting the input value
#'   according to the specified locale's rules. Example locale names include
#'   `"en_US"` for English (United States) and `"fr_FR"` for French (France). If
#'   a locale isn't provided and certain locale-based formatting is to be done
#'   then the `"en_US"` locale will be used.
#'
#' @export
fmt_dt <- function(
    input,
    format = NULL,
    use_tz = NULL,
    locale = NULL
) {

  if (is.null(locale)) {
    locale <- "en"
  }

  # Initialize an empty `tz_info` list
  tz_info <-
    list(
      tz_str = NA_character_,
      tz_offset = NA_real_,
      long_tzid = NA_character_,
      tz_short_specific = NA_character_,
      tz_long_specific = NA_character_
    )

  # Modify the `format` string so it can be more precisely formatted
  pattern_list <- dt_format_pattern(format = format)

  if (is.character(input)) {

    date_present <- is_date_present(input = input)
    time_present <- is_time_present(input = input)

    # Determine if tz information is present, either as:
    # [1] a tz offset in hours from GMT
    # [2] a long tz identifier (either canonical or an alias)

    tz_present <- is_tz_present(input = input)
    long_tzid_present <- is_long_tzid_present(input = input)

    # Strip away tz information from the input and return as `input_str`
    if (tz_present) {
      input_str <- strip_tz(input = input)
    } else if (long_tzid_present) {
      input_str <- strip_long_tzid(input = input)
    } else {
      input_str <- input
    }

    # Obtain the date and time
    if (date_present) {

      input_dt <- as.POSIXct(gsub("T", " ", input_str), tz = "UTC")

    } else if (!date_present && time_present) {

      date_now <- as.character(Sys.Date())
      input_str <- paste0(date_now, "T", input_str)
      input_dt <- as.POSIXct(gsub("T", " ", input_str), tz = "UTC")
    }

    # Derive more detailed time zone information from the `long_tzid` value
    if (!is.null(use_tz)) {

      # Validate and then normalize the provided time zone
      validate_long_tzid(long_tzid = use_tz)
      long_tzid <- normalize_long_tzid(long_tzid = use_tz)

      tz_info$long_tzid <- long_tzid
      tz_info$tz_str <- long_tzid_to_tz_str(long_tzid = tz_info$long_tzid, input_dt = input_dt)
      tz_info$tz_offset <- get_tz_offset_val_from_tz_str(tz_str = tz_info$tz_str)
      tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
      tz_info$tz_long_specific <- get_tz_long_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt, locale = locale)

    } else if (long_tzid_present) {

      tz_info$long_tzid <- get_long_tzid_str(input = input)
      tz_info$tz_str <- long_tzid_to_tz_str(long_tzid = tz_info$long_tzid, input_dt = input_dt)
      tz_info$tz_offset <- get_tz_offset_val_from_tz_str(tz_str = tz_info$tz_str)
      tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
      tz_info$tz_long_specific <- get_tz_long_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt, locale = locale)

    } else if (tz_present) {

      tz_info$tz_str <- get_tz_str(input = input)
      tz_info$tz_offset <- get_tz_offset_val(input = input)

    } else {

      tz_info$long_tzid <- "UTC"
      tz_info$tz_str <- "GMT"
      tz_info$tz_offset <- 0L
      tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
    }
  }

  if (inherits(input, "POSIXct")) {

    input_dt <- input

    # Extract the input time zone
    tz_info$tz_str <- attr(input_dt, which = "tzone", exact = TRUE)
  }

  dt_lett <- pattern_list$dt_letters
  dt <- pattern_list$format

  if ("G" %in% dt_lett) {
    dt <- gsub("{G}", dt_G(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{GG}", dt_G(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{GGG}", dt_G(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{GGGG}", dt_GGGG(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{GGGGG}", dt_GGGGG(input_dt, locale), dt, fixed = TRUE)
  }

  if ("y" %in% dt_lett) {
    dt <- gsub("{y}", dt_y(input_dt), dt, fixed = TRUE)
    dt <- gsub("{yy}", dt_yy(input_dt), dt, fixed = TRUE)
    dt <- gsub("{yyy}", dt_yyy(input_dt), dt, fixed = TRUE)
    dt <- gsub("{yyyy}", dt_yyyy(input_dt), dt, fixed = TRUE)
    dt <- gsub("{yyyyy}", dt_yyyyy(input_dt), dt, fixed = TRUE)
    dt <- gsub("{yyyyyy}", dt_yyyyyy(input_dt), dt, fixed = TRUE)
    dt <- gsub("{yyyyyyy}", dt_yyyyyyy(input_dt), dt, fixed = TRUE)
    dt <- gsub("{yyyyyyyy}", dt_yyyyyyyy(input_dt), dt, fixed = TRUE)
    dt <- gsub("{yyyyyyyyy}", dt_yyyyyyyyy(input_dt), dt, fixed = TRUE)
  }

  if ("Y" %in% dt_lett) {
    dt <- gsub("{Y}", dt_Y(input_dt), dt, fixed = TRUE)
    dt <- gsub("{YY}", dt_YY(input_dt), dt, fixed = TRUE)
  }

  dt <- gsub("{u}", dt_u(input_dt), dt, fixed = TRUE)

  if ("U" %in% dt_lett) {
    dt <- gsub("{U}", dt_U(input_dt), dt, fixed = TRUE)
    dt <- gsub("{UU}", dt_U(input_dt), dt, fixed = TRUE)
    dt <- gsub("{UUU}", dt_U(input_dt), dt, fixed = TRUE)
    dt <- gsub("{UUUU}", dt_UUUU(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{UUUUU}", dt_UUUUU(input_dt, locale), dt, fixed = TRUE)
  }

  if ("Q" %in% dt_lett) {
    dt <- gsub("{Q}", dt_Q(input_dt), dt, fixed = TRUE)
    dt <- gsub("{QQ}", dt_QQ(input_dt), dt, fixed = TRUE)
    dt <- gsub("{QQQ}", dt_QQQ(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{QQQQ}", dt_QQQQ(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{QQQQQ}", dt_QQQQQ(input_dt, locale), dt, fixed = TRUE)
  }

  if ("q" %in% dt_lett) {
    dt <- gsub("{q}", dt_q(input_dt), dt, fixed = TRUE)
    dt <- gsub("{qq}", dt_qq(input_dt), dt, fixed = TRUE)
    dt <- gsub("{qqq}", dt_qqq(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{qqqq}", dt_qqqq(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{qqqqq}", dt_qqqqq(input_dt, locale), dt, fixed = TRUE)
  }

  if ("M" %in% dt_lett) {
    dt <- gsub("{M}", dt_M(input_dt), dt, fixed = TRUE)
    dt <- gsub("{MM}", dt_MM(input_dt), dt, fixed = TRUE)
    dt <- gsub("{MMM}", dt_MMM(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{MMMM}", dt_MMMM(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{MMMMM}", dt_MMMMM(input_dt, locale), dt, fixed = TRUE)
  }

  if ("L" %in% dt_lett) {
    dt <- gsub("{L}", dt_L(input_dt), dt, fixed = TRUE)
    dt <- gsub("{LL}", dt_LL(input_dt), dt, fixed = TRUE)
    dt <- gsub("{LLL}", dt_LLL(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{LLLL}", dt_LLLL(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{LLLLL}", dt_LLLLL(input_dt, locale), dt, fixed = TRUE)
  }

  if ("w" %in% dt_lett) {
    dt <- gsub("{w}", dt_w(input_dt), dt, fixed = TRUE)
    dt <- gsub("{ww}", dt_ww(input_dt), dt, fixed = TRUE)
  }

  dt <- gsub("{W}", dt_W(input_dt), dt, fixed = TRUE)

  if ("d" %in% dt_lett) {
    dt <- gsub("{d}", dt_d(input_dt), dt, fixed = TRUE)
    dt <- gsub("{dd}", dt_dd(input_dt), dt, fixed = TRUE)
  }

  if ("D" %in% dt_lett) {
    dt <- gsub("{D}", dt_D(input_dt), dt, fixed = TRUE)
    dt <- gsub("{DD}", dt_DD(input_dt), dt, fixed = TRUE)
    dt <- gsub("{DDD}", dt_DDD(input_dt), dt, fixed = TRUE)
  }

  dt <- gsub("{F}", dt_F(input_dt), dt, fixed = TRUE)

  if ("E" %in% dt_lett) {
    dt <- gsub("{E}", dt_E(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{EE}", dt_E(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{EEE}", dt_E(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{EEEE}", dt_EEEE(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{EEEEE}", dt_EEEEE(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{EEEEEE}", dt_EEEEEE(input_dt, locale), dt, fixed = TRUE)
  }

  if ("e" %in% dt_lett) {
    dt <- gsub("{e}", dt_e(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{ee}", dt_ee(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{eee}", dt_eee(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{eeee}", dt_eeee(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{eeeee}", dt_eeeee(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{eeeeee}", dt_eeeeee(input_dt, locale), dt, fixed = TRUE)
  }

  if ("c" %in% dt_lett) {
    dt <- gsub("{c}", dt_c(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{cc}", dt_cc(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{ccc}", dt_ccc(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{cccc}", dt_cccc(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{ccccc}", dt_ccccc(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{cccccc}", dt_cccccc(input_dt, locale), dt, fixed = TRUE)
  }

  if ("a" %in% dt_lett) {
    dt <- gsub("{a}", dt_a(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{aa}", dt_a(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{aaa}", dt_a(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{aaaa}", dt_aaaa(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{aaaaa}", dt_aaaaa(input_dt, locale), dt, fixed = TRUE)
  }

  if ("b" %in% dt_lett) {
    dt <- gsub("{b}", dt_b(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{bb}", dt_b(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{bbb}", dt_b(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{bbbb}", dt_bbbb(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{bbbbb}", dt_bbbbb(input_dt, locale), dt, fixed = TRUE)
  }

  if ("B" %in% dt_lett) {
    dt <- gsub("{B}", dt_B(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{BB}", dt_B(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{BBB}", dt_B(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{BBBB}", dt_BBBB(input_dt, locale), dt, fixed = TRUE)
    dt <- gsub("{BBBBB}", dt_BBBBB(input_dt, locale), dt, fixed = TRUE)
  }

  if ("h" %in% dt_lett) {
    dt <- gsub("{h}", dt_h(input_dt), dt, fixed = TRUE)
    dt <- gsub("{hh}", dt_hh(input_dt), dt, fixed = TRUE)
  }

  if ("H" %in% dt_lett) {
    dt <- gsub("{H}", dt_H(input_dt), dt, fixed = TRUE)
    dt <- gsub("{HH}", dt_HH(input_dt), dt, fixed = TRUE)
  }

  if ("k" %in% dt_lett) {
    dt <- gsub("{k}", dt_k(input_dt), dt, fixed = TRUE)
    dt <- gsub("{kk}", dt_kk(input_dt), dt, fixed = TRUE)
  }

  if ("K" %in% dt_lett) {
    dt <- gsub("{K}", dt_K(input_dt), dt, fixed = TRUE)
    dt <- gsub("{KK}", dt_KK(input_dt), dt, fixed = TRUE)
  }

  if ("m" %in% dt_lett) {
    dt <- gsub("{m}", dt_m(input_dt), dt, fixed = TRUE)
    dt <- gsub("{mm}", dt_mm(input_dt), dt, fixed = TRUE)
  }

  if ("s" %in% dt_lett) {
    dt <- gsub("{s}", dt_s(input_dt), dt, fixed = TRUE)
    dt <- gsub("{ss}", dt_ss(input_dt), dt, fixed = TRUE)
  }

  if ("z" %in% dt_lett) {
    dt <- gsub("{z}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{zz}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{zzz}", dt_z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{zzzz}", dt_zzzz(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("Z" %in% dt_lett) {
    dt <- gsub("{Z}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{ZZ}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{ZZZ}", dt_Z(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{ZZZZ}", dt_ZZZZ(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{ZZZZZ}", dt_ZZZZZ(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("O" %in% dt_lett) {
    dt <- gsub("{O}", dt_O(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{OOOO}", dt_OOOO(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("v" %in% dt_lett) {
    dt <- gsub("{v}", dt_v(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{vvvv}", dt_vvvv(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("V" %in% dt_lett) {
    dt <- gsub("{V}", dt_V(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{VV}", dt_VV(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{VVV}", dt_VVV(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{VVVV}", dt_VVVV(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("X" %in% dt_lett) {
    dt <- gsub("{X}", dt_X(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{XX}", dt_XX(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{XXX}", dt_XXX(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{XXXX}", dt_XXXX(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{XXXXX}", dt_XXXXX(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("x" %in% dt_lett) {
    dt <- gsub("{x}", dt_x(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{xx}", dt_xx(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{xxx}", dt_xxx(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{xxxx}", dt_xxxx(input_dt, tz_info, locale), dt, fixed = TRUE)
    dt <- gsub("{xxxxx}", dt_xxxxx(input_dt, tz_info, locale), dt, fixed = TRUE)
  }

  if ("g" %in% dt_lett) {
    dt <- gsub("{g}", dt_g(input_dt), dt, fixed = TRUE)
    dt <- gsub("{gg}", dt_gg(input_dt), dt, fixed = TRUE)
    dt <- gsub("{ggg}", dt_ggg(input_dt), dt, fixed = TRUE)
    dt <- gsub("{gggg}", dt_gggg(input_dt), dt, fixed = TRUE)
    dt <- gsub("{ggggg}", dt_ggggg(input_dt), dt, fixed = TRUE)
    dt <- gsub("{gggggg}", dt_gggggg(input_dt), dt, fixed = TRUE)
    dt <- gsub("{ggggggg}", dt_ggggggg(input_dt), dt, fixed = TRUE)
    dt <- gsub("{gggggggg}", dt_gggggggg(input_dt), dt, fixed = TRUE)
    dt <- gsub("{ggggggggg}", dt_ggggggggg(input_dt), dt, fixed = TRUE)
  }

  dt <- gsub("\\{(.*?)\\}", "\\1", dt)

  # Replace string literal markers `'<#>'` with captured literal values
  for (i in seq_along(pattern_list$literals)) {

    dt <-
      gsub(
        paste0("'", i, "'"),
        pattern_list$literals[i],
        dt,
        fixed = TRUE
      )
  }

  # Replace each instance of `''` with `'`
  dt <- gsub("''", "'", dt, fixed = TRUE)

  dt
}

sub_letters <- function() {
  c(
    "G",
    "y",
    "Y",
    "u",
    "U",
    "r",
    "Q",
    "q",
    "M",
    "L",
    "w",
    "W",
    "d",
    "D",
    "F",
    "g",
    "E",
    "e",
    "c",
    "a",
    "b",
    "B",
    "h",
    "H",
    "K",
    "k",
    "m",
    "s",
    "S",
    "A",
    "z",
    "Z",
    "O",
    "v",
    "V",
    "X",
    "x"
  )
}

extract_literals_from_pattern <- function(string) {

  gsub(
    "(^'|'$)", "",
    unlist(regmatches(string, gregexpr(pattern = "'.*?'", text = string)))
  )
}

dt_format_pattern <- function(format) {

  literals <- extract_literals_from_pattern(string = format)

  for (i in seq_along(literals)) {
    if (literals[i] != "") {
      format <- sub(literals[i], i, format)
    }
  }

  dt_letters <-
    base::intersect(
      unique(unlist(strsplit(format, ""))),
      sub_letters()
    )

  pattern <- paste0("(", paste0("[", dt_letters, "]+", collapse = "|"), ")")

  format <- gsub(pattern, "\\{\\1\\}", format)

  literals <- gsub("(^'|'$)", "", literals)

  list(
    format = format,
    literals = literals,
    dt_letters = dt_letters
  )
}
