#' Format a datetime using a SDF formatting string
#'
#' @param input The input date-time. The appropriate representation should use
#'   the following construction outlined in the ISO 8601:2004 standard:
#'   `YYYY-MM-DDThh:mm:ss.sTZD` although some allowances made here to ease this
#'   restrictiveness (for example, the literal `T` separating the date and time
#'   components is optional). Fractional seconds are optional as is the
#'   time-zone designation (TZD).
#' @param date_format a date format specification using the rules of the
#'   SimpleDateFormat.
#' @param time_format a time format specification using the rules of the
#'   SimpleDateFormat.
#' @param combination a combining pattern for the localized date and time
#'   components. If this is not provided, then the combining pattern will come
#'   from the specified locale's `"full"` designation.
#' @param locale The output locale to use for formatting the `input`
#'   value according to the specified locale's rules. Example locale names
#'   include `"en_US"` for English (United States) and `"fr_FR"` for French
#'   (France). If a locale isn't provided and certain require locale- based
#'   formatting then the `"en_US"` locale is used for this purpose.
#'
#' @export
fmt_dt <- function(
    input,
    dt_format = NULL,
    use_tz = NULL,
    locale = NULL
) {

  if (is.null(locale)) {
    locale <- "en"
  }

  # Modify the `dt_format` string so it is `glue_dt()` formattable
  pattern_list <- dt_format_to_glue_pattern(dt_format = dt_format)

  if (is.character(input)) {

    date_present <- is_date_present(input = input)
    time_present <- is_time_present(input = input)

    # Extract the time zone offset if it exists and strip
    # any tz information from the input
    if (is_tz_present(input = input)) {

      tz_str <- get_tz_str(input = input)
      tz_offset <- get_tz_offset_val(input = input)
      input <- strip_tz(input = input)
      tz_short_specific <- get_tz_short_specific(tz_str = tz_str)

    } else {

      tz_offset <- NA_real_
      tz_str <- NA_character_
      tz_short_specific <- NA_character_
    }

    if (date_present && time_present) {
      input_dt <- lubridate::ymd_hms(input, tz = "UTC", quiet = TRUE)
    } else if (date_present && !time_present) {
      input_dt <- lubridate::ymd(input, tz = "UTC", quiet = TRUE)
    } else if (!date_present && time_present) {
      input <- paste0("2015-01-01T", input)
      input_dt <- lubridate::ymd_hms(input, tz = "UTC",  quiet = TRUE)
    }

  } else if (inherits(input, "POSIXct")) {

    input_dt <- input

    # Extract the input time zone
    tz_str <- attr(input_dt, which = "tzone", exact = TRUE)
    tz_offset <- NULL
  }

  # Prepare the output vector
  output <- rep(NA_character_, length(input))

  output_dt <-
    glue_dt(
      list(
        G = dt_G(input, locale),
        GG  = dt_G(input, locale),
        GGG = dt_G(input, locale),
        GGGG = dt_GGGG(input, locale),
        GGGGG = dt_GGGGG(input, locale),
        y = dt_y(input),
        yy = dt_yy(input),
        Y = dt_Y(input),
        YY = dt_YY(input),
        u = dt_u(input),
        U = dt_U(input),
        UU = dt_U(input),
        UUU = dt_U(input),
        UUUU = dt_UUUU(input, locale),
        UUUUU = dt_UUUUU(input, locale),
        Q = dt_Q(input),
        QQ = dt_QQ(input),
        QQQ = dt_QQQ(input, locale),
        QQQQ = dt_QQQQ(input, locale),
        QQQQQ = dt_QQQQQ(input, locale),
        q = dt_q(input),
        qq = dt_qq(input),
        qqq = dt_qqq(input, locale),
        qqqq = dt_qqqq(input, locale),
        qqqqq = dt_qqqqq(input, locale),
        M = dt_M(input),
        MM = dt_MM(input),
        MMM = dt_MMM(input, locale),
        MMMM = dt_MMMM(input, locale),
        MMMMM = dt_MMMMM(input, locale),
        L = dt_L(input),
        LL = dt_LL(input),
        LLL = dt_LLL(input, locale),
        LLLL = dt_LLLL(input, locale),
        LLLLL = dt_LLLLL(input, locale),
        w = dt_w(input),
        ww = dt_ww(input),
        W = dt_W(input),
        d = dt_d(input),
        dd = dt_dd(input),
        D = dt_D(input),
        DD = dt_DD(input),
        DDD = dt_DDD(input),
        F = dt_F(input),
        E = dt_E(input, locale),
        EE = dt_E(input, locale),
        EEE = dt_E(input, locale),
        EEEE = dt_EEEE(input, locale),
        EEEEE = dt_EEEEE(input, locale),
        EEEEEE = dt_EEEEEE(input, locale),
        e = dt_e(input, locale),
        ee = dt_ee(input, locale),
        eee = dt_eee(input, locale),
        eeee = dt_eeee(input, locale),
        eeeee = dt_eeeee(input, locale),
        eeeeee = dt_eeeeee(input, locale),
        c = dt_c(input, locale),
        cc = dt_cc(input, locale),
        ccc = dt_ccc(input, locale),
        cccc = dt_cccc(input, locale),
        ccccc = dt_ccccc(input, locale),
        cccccc = dt_cccccc(input, locale),
        a = dt_a(input, locale),
        aa = dt_a(input, locale),
        aaa = dt_a(input, locale),
        aaaa = dt_aaaa(input, locale),
        aaaaa = dt_aaaaa(input, locale),
        b = dt_b(input, locale),
        bb = dt_b(input, locale),
        bbb = dt_b(input, locale),
        bbbb = dt_bbbb(input, locale),
        bbbbb = dt_bbbbb(input, locale),
        B = dt_B(input, locale),
        BB = dt_B(input, locale),
        BBB = dt_B(input, locale),
        BBBB = dt_BBBB(input, locale),
        BBBBB = dt_BBBBB(input, locale),
        h = dt_h(input),
        hh = dt_hh(input),
        H = dt_H(input),
        HH = dt_HH(input),
        K = dt_K(input),
        KK = dt_KK(input),
        k = dt_k(input),
        kk = dt_kk(input),
        m = dt_m(input),
        mm = dt_mm(input),
        s = dt_s(input),
        ss = dt_ss(input),
        z = dt_z(input, locale),
        zz = dt_z(input, locale),
        zzz = dt_z(input, locale),
        zzzz = dt_zzzz(input, locale),
        Z = dt_Z(input, locale),
        ZZ = dt_Z(input, locale),
        ZZZ = dt_Z(input, locale),
        ZZZZ = dt_ZZZZ(input, locale),
        ZZZZZ = dt_ZZZZZ(input, locale),
        O = dt_O(input, locale, tz_offset),
        OOOO = dt_OOOO(input, locale, tz_offset),
        v = dt_v(input, locale),
        vvvv = dt_vvvv(input, locale),
        V = dt_V(input, locale),
        VV = dt_VV(input, locale),
        VVV = dt_VVV(input, locale),
        VVVV = dt_VVVV(input, locale),
        X = dt_X(input, locale),
        XX = dt_XX(input, locale),
        XXX = dt_XXX(input, locale),
        XXXX = dt_XXXX(input, locale),
        XXXXX = dt_XXXXX(input, locale),
        x = dt_x(input, locale),
        xx = dt_xx(input, locale),
        xxx = dt_xxx(input, locale),
        xxxx = dt_xxxx(input, locale),
        xxxxx = dt_xxxxx(input, locale)
      ),
      pattern_list$dt_format
    )

  for (i in seq_along(pattern_list$literals)) {

    output_dt <-
      gsub(
        paste0("'", i, "'"),
        pattern_list$literals[i],
        output_dt,
        fixed = TRUE
      )
  }

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

dt_format_to_glue_pattern <- function(dt_format) {

  literals <-
    extract_literals_from_pattern(
      string = dt_format
    )

  for (i in seq_along(literals)) {
    dt_format <- sub(literals[i], paste0("'", i, "'"), dt_format)
  }

  dt_letters <-
    base::intersect(
      unique(unlist(strsplit(dt_format, ""))),
      sub_letters()
    )

  pattern <- paste0("(", paste0("[", dt_letters, "]+", collapse = "|"), ")")

  dt_format <- gsub(pattern, "\\{\\1\\}", dt_format)

  literals <- gsub("(^'|'$)", "", literals)

  list(
    dt_format = dt_format,
    literals = literals
  )
}

glue_dt <- function (.x, ...) {
  glue::glue_data(.x, ..., .transformer = get, .envir = emptyenv())
}
