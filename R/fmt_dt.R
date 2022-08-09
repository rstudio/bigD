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

  # Modify the `format` string so it is `glue_dt()` formattable
  pattern_list <- dt_format_to_glue_pattern(format = format)

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
    if (date_present && time_present) {
      if (grepl("(T| )[0-2][0-9]:[0-5][0-9]:[0-5][0-9]", input_str)) {
        input_dt <- lubridate::ymd_hms(input_str, tz = "UTC", quiet = TRUE)
      } else {
        input_dt <- lubridate::ymd_hm(input_str, tz = "UTC", quiet = TRUE)
      }
    } else if (date_present && !time_present) {
      input_dt <- lubridate::ymd(input_str, tz = "UTC", quiet = TRUE)
    } else if (!date_present && time_present) {
      input_str <- paste0("2015-01-01T", input_str)
      input_dt <- lubridate::ymd_hms(input_str, tz = "UTC",  quiet = TRUE)
    }

    # Derive more detailed time zone information from the `long_tzid` value
    if (long_tzid_present) {

      tz_info$long_tzid <- get_long_tzid_str(input = input)
      tz_info$tz_str <- long_tzid_to_tz_str(long_tzid = tz_info$long_tzid, input_dt = input_dt)
      tz_info$tz_offset <- get_tz_offset_val_from_tz_str(tz_str = tz_info$tz_str)
      tz_info$tz_short_specific <- get_tz_short_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt)
      tz_info$tz_long_specific <- get_tz_long_specific(long_tzid = tz_info$long_tzid, input_dt = input_dt, locale = locale)

    } else if (tz_present) {

      tz_info$tz_str <- get_tz_str(input = input)
      tz_info$tz_offset <- get_tz_offset_val(input = input)
    }
  }

  if (inherits(input, "POSIXct")) {

    input_dt <- input

    # Extract the input time zone
    tz_info$tz_str <- attr(input_dt, which = "tzone", exact = TRUE)
  }

  output_dt <-
    glue_dt(
      list(
        G = dt_G(input_dt, locale),
        GG  = dt_G(input_dt, locale),
        GGG = dt_G(input_dt, locale),
        GGGG = dt_GGGG(input_dt, locale),
        GGGGG = dt_GGGGG(input_dt, locale),
        y = dt_y(input_dt),
        yy = dt_yy(input_dt),
        yyy = dt_yyy(input_dt),
        yyyy = dt_yyyy(input_dt),
        yyyyy = dt_yyyyy(input_dt),
        yyyyyy = dt_yyyyyy(input_dt),
        yyyyyyy = dt_yyyyyyy(input_dt),
        yyyyyyyy = dt_yyyyyyyy(input_dt),
        yyyyyyyyy = dt_yyyyyyyyy(input_dt),
        Y = dt_Y(input_dt),
        YY = dt_YY(input_dt),
        u = dt_u(input_dt),
        U = dt_U(input_dt),
        UU = dt_U(input_dt),
        UUU = dt_U(input_dt),
        UUUU = dt_UUUU(input_dt, locale),
        UUUUU = dt_UUUUU(input_dt, locale),
        Q = dt_Q(input_dt),
        QQ = dt_QQ(input_dt),
        QQQ = dt_QQQ(input_dt, locale),
        QQQQ = dt_QQQQ(input_dt, locale),
        QQQQQ = dt_QQQQQ(input_dt, locale),
        q = dt_q(input_dt),
        qq = dt_qq(input_dt),
        qqq = dt_qqq(input_dt, locale),
        qqqq = dt_qqqq(input_dt, locale),
        qqqqq = dt_qqqqq(input_dt, locale),
        M = dt_M(input_dt),
        MM = dt_MM(input_dt),
        MMM = dt_MMM(input_dt, locale),
        MMMM = dt_MMMM(input_dt, locale),
        MMMMM = dt_MMMMM(input_dt, locale),
        L = dt_L(input_dt),
        LL = dt_LL(input_dt),
        LLL = dt_LLL(input_dt, locale),
        LLLL = dt_LLLL(input_dt, locale),
        LLLLL = dt_LLLLL(input_dt, locale),
        w = dt_w(input_dt),
        ww = dt_ww(input_dt),
        W = dt_W(input_dt),
        d = dt_d(input_dt),
        dd = dt_dd(input_dt),
        D = dt_D(input_dt),
        DD = dt_DD(input_dt),
        DDD = dt_DDD(input_dt),
        F = dt_F(input_dt),
        E = dt_E(input_dt, locale),
        EE = dt_E(input_dt, locale),
        EEE = dt_E(input_dt, locale),
        EEEE = dt_EEEE(input_dt, locale),
        EEEEE = dt_EEEEE(input_dt, locale),
        EEEEEE = dt_EEEEEE(input_dt, locale),
        e = dt_e(input_dt, locale),
        ee = dt_ee(input_dt, locale),
        eee = dt_eee(input_dt, locale),
        eeee = dt_eeee(input_dt, locale),
        eeeee = dt_eeeee(input_dt, locale),
        eeeeee = dt_eeeeee(input_dt, locale),
        c = dt_c(input_dt, locale),
        cc = dt_cc(input_dt, locale),
        ccc = dt_ccc(input_dt, locale),
        cccc = dt_cccc(input_dt, locale),
        ccccc = dt_ccccc(input_dt, locale),
        cccccc = dt_cccccc(input_dt, locale),
        a = dt_a(input_dt, locale),
        aa = dt_a(input_dt, locale),
        aaa = dt_a(input_dt, locale),
        aaaa = dt_aaaa(input_dt, locale),
        aaaaa = dt_aaaaa(input_dt, locale),
        b = dt_b(input_dt, locale),
        bb = dt_b(input_dt, locale),
        bbb = dt_b(input_dt, locale),
        bbbb = dt_bbbb(input_dt, locale),
        bbbbb = dt_bbbbb(input_dt, locale),
        B = dt_B(input_dt, locale),
        BB = dt_B(input_dt, locale),
        BBB = dt_B(input_dt, locale),
        BBBB = dt_BBBB(input_dt, locale),
        BBBBB = dt_BBBBB(input_dt, locale),
        h = dt_h(input_dt),
        hh = dt_hh(input_dt),
        H = dt_H(input_dt),
        HH = dt_HH(input_dt),
        K = dt_K(input_dt),
        KK = dt_KK(input_dt),
        k = dt_k(input_dt),
        kk = dt_kk(input_dt),
        m = dt_m(input_dt),
        mm = dt_mm(input_dt),
        s = dt_s(input_dt),
        ss = dt_ss(input_dt),
        z = dt_z(input_dt, tz_info, locale),
        zz = dt_z(input_dt, tz_info, locale),
        zzz = dt_z(input_dt, tz_info, locale),
        zzzz = dt_zzzz(input_dt, tz_info, locale),
        Z = dt_Z(input_dt, tz_info, locale),
        ZZ = dt_Z(input_dt, tz_info, locale),
        ZZZ = dt_Z(input_dt, tz_info, locale),
        ZZZZ = dt_ZZZZ(input_dt, tz_info, locale),
        ZZZZZ = dt_ZZZZZ(input_dt, tz_info, locale),
        O = dt_O(input_dt, tz_info, locale),
        OOOO = dt_OOOO(input_dt, tz_info, locale),
        v = dt_v(input_dt, tz_info, locale),
        vvvv = dt_vvvv(input_dt, tz_info, locale),
        V = dt_V(input_dt, tz_info, locale),
        VV = dt_VV(input_dt, tz_info, locale),
        VVV = dt_VVV(input_dt, tz_info, locale),
        VVVV = dt_VVVV(input_dt, tz_info, locale),
        X = dt_X(input_dt, tz_info, locale),
        XX = dt_XX(input_dt, tz_info, locale),
        XXX = dt_XXX(input_dt, tz_info, locale),
        XXXX = dt_XXXX(input_dt, tz_info, locale),
        XXXXX = dt_XXXXX(input_dt, tz_info, locale),
        x = dt_x(input_dt, tz_info, locale),
        xx = dt_xx(input_dt, tz_info, locale),
        xxx = dt_xxx(input_dt, tz_info, locale),
        xxxx = dt_xxxx(input_dt, tz_info, locale),
        xxxxx = dt_xxxxx(input_dt, tz_info, locale)
      ),
      pattern_list$format
    )

  # Replace string literal markers `'<#>'` with captured literal values
  for (i in seq_along(pattern_list$literals)) {

    output_dt <-
      gsub(
        paste0("'", i, "'"),
        pattern_list$literals[i],
        output_dt,
        fixed = TRUE
      )
  }

  # Replace each instance of `''` with `'`
  output_dt <- gsub("''", "'", output_dt, fixed = TRUE)

  output_dt
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

dt_format_to_glue_pattern <- function(format) {

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
    literals = literals
  )
}

glue_dt <- function (.x, ...) {
  as.character(
    glue::glue_data(.x, ..., .transformer = get, .envir = emptyenv())
  )
}
