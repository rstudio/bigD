#' Get a vector of all supported locales
#'
#' @description
#' The `fdt_locales_vec()` function produces a vector of all supported locale
#' IDs in the **bigD** package.
#'
#' @return
#' A character vector of supported locale IDs.
#'
#' @examples
#' # Let's get all the `ar` locales that exist
#' # in the vector produced by `fdt_locales_vec()`
#' fdt_locales_vec()[grep("^ar", fdt_locales_vec())]
#'
#' # Let's get all the locales that pertain to the
#' # `CH` territory in the vector produced by
#' # `fdt_locales_vec()`
#' fdt_locales_vec()[grep("CH", fdt_locales_vec())]
#'
#' @export
fdt_locales_vec <- function() {
  c(
    "af", "af_NA", "agq", "ak", "am", "ar", "ar_AE", "ar_BH", "ar_DJ",
    "ar_DZ", "ar_EG", "ar_EH", "ar_ER", "ar_IL", "ar_IQ", "ar_JO",
    "ar_KM", "ar_KW", "ar_LB", "ar_LY", "ar_MA", "ar_MR", "ar_OM",
    "ar_PS", "ar_QA", "ar_SA", "ar_SD", "ar_SO", "ar_SS", "ar_SY",
    "ar_TD", "ar_TN", "ar_YE", "as", "asa", "ast", "az", "az_Cyrl",
    "az_Latn", "bas", "be", "be_tarask", "bem", "bez", "bg", "bm",
    "bn", "bn_IN", "bo", "bo_IN", "br", "brx", "bs", "bs_Cyrl", "bs_Latn",
    "ca", "ca_AD", "ca_ES_valencia", "ca_FR", "ca_IT", "ccp", "ccp_IN",
    "ce", "ceb", "cgg", "chr", "ckb", "ckb_IR", "cs", "cy", "da",
    "da_GL", "dav", "de", "de_AT", "de_BE", "de_CH", "de_IT", "de_LI",
    "de_LU", "dje", "doi", "dsb", "dua", "dyo", "dz", "ebu", "ee",
    "ee_TG", "el", "el_CY", "en", "en_001", "en_150", "en_AE", "en_AG",
    "en_AI", "en_AS", "en_AT", "en_AU", "en_BB", "en_BE", "en_BI",
    "en_BM", "en_BS", "en_BW", "en_BZ", "en_CA", "en_CC", "en_CH",
    "en_CK", "en_CM", "en_CX", "en_CY", "en_DE", "en_DG", "en_DK",
    "en_DM", "en_ER", "en_FI", "en_FJ", "en_FK", "en_FM", "en_GB",
    "en_GD", "en_GG", "en_GH", "en_GI", "en_GM", "en_GU", "en_GY",
    "en_HK", "en_IE", "en_IL", "en_IM", "en_IN", "en_IO", "en_JE",
    "en_JM", "en_KE", "en_KI", "en_KN", "en_KY", "en_LC", "en_LR",
    "en_LS", "en_MG", "en_MH", "en_MO", "en_MP", "en_MS", "en_MT",
    "en_MU", "en_MV", "en_MW", "en_MY", "en_NA", "en_NF", "en_NG",
    "en_NL", "en_NR", "en_NU", "en_NZ", "en_PG", "en_PH", "en_PK",
    "en_PN", "en_PR", "en_PW", "en_RW", "en_SB", "en_SC", "en_SD",
    "en_SE", "en_SG", "en_SH", "en_SI", "en_SL", "en_SS", "en_SX",
    "en_SZ", "en_TC", "en_TK", "en_TO", "en_TT", "en_TV", "en_TZ",
    "en_UG", "en_UM", "en_VC", "en_VG", "en_VI", "en_VU", "en_WS",
    "en_ZA", "en_ZM", "en_ZW", "eo", "es", "es_419", "es_AR", "es_BO",
    "es_BR", "es_BZ", "es_CL", "es_CO", "es_CR", "es_CU", "es_DO",
    "es_EA", "es_EC", "es_GQ", "es_GT", "es_HN", "es_IC", "es_MX",
    "es_NI", "es_PA", "es_PE", "es_PH", "es_PR", "es_PY", "es_SV",
    "es_US", "es_UY", "es_VE", "et", "eu", "ewo", "fa", "fa_AF",
    "ff", "ff_Adlm", "ff_Adlm_BF", "ff_Adlm_CM", "ff_Adlm_GH", "ff_Adlm_GM",
    "ff_Adlm_GW", "ff_Adlm_LR", "ff_Adlm_MR", "ff_Adlm_NE", "ff_Adlm_NG",
    "ff_Adlm_SL", "ff_Adlm_SN", "ff_Latn", "ff_Latn_BF", "ff_Latn_CM",
    "ff_Latn_GH", "ff_Latn_GM", "ff_Latn_GN", "ff_Latn_GW", "ff_Latn_LR",
    "ff_Latn_MR", "ff_Latn_NE", "ff_Latn_NG", "ff_Latn_SL", "fi",
    "fil", "fo", "fo_DK", "fr", "fr_BE", "fr_BF", "fr_BI", "fr_BJ",
    "fr_BL", "fr_CA", "fr_CD", "fr_CF", "fr_CG", "fr_CH", "fr_CI",
    "fr_CM", "fr_DJ", "fr_DZ", "fr_GA", "fr_GF", "fr_GN", "fr_GP",
    "fr_GQ", "fr_HT", "fr_KM", "fr_LU", "fr_MA", "fr_MC", "fr_MF",
    "fr_MG", "fr_ML", "fr_MQ", "fr_MR", "fr_MU", "fr_NC", "fr_NE",
    "fr_PF", "fr_PM", "fr_RE", "fr_RW", "fr_SC", "fr_SN", "fr_SY",
    "fr_TD", "fr_TG", "fr_TN", "fr_VU", "fr_WF", "fr_YT", "fur",
    "fy", "ga", "ga_GB", "gd", "gl", "gsw", "gsw_FR", "gsw_LI", "gu",
    "guz", "gv", "ha", "ha_GH", "ha_NE", "haw", "he", "hi", "hi_Latn",
    "hr", "hr_BA", "hsb", "hu", "hy", "ia", "id", "ig", "ii", "is",
    "it", "it_CH", "it_SM", "it_VA", "ja", "jgo", "jmc", "jv", "ka",
    "kab", "kam", "kde", "kea", "kgp", "khq", "ki", "kk", "kkj",
    "kl", "kln", "km", "kn", "ko", "ko_KP", "kok", "ks", "ks_Arab",
    "ks_Deva", "ksb", "ksf", "ksh", "ku", "kw", "ky", "lag", "lb",
    "lg", "lkt", "ln", "ln_AO", "ln_CF", "ln_CG", "lo", "lrc", "lrc_IQ",
    "lt", "lu", "luo", "luy", "lv", "mai", "mas", "mas_TZ", "mer",
    "mfe", "mg", "mgh", "mgo", "mi", "mk", "ml", "mn", "mni", "mni_Beng",
    "mr", "ms", "ms_BN", "ms_ID", "ms_SG", "mt", "mua", "my", "mzn",
    "naq", "nb", "nb_SJ", "nd", "nds", "nds_NL", "ne", "ne_IN", "nl",
    "nl_AW", "nl_BE", "nl_BQ", "nl_CW", "nl_SR", "nl_SX", "nmg",
    "nn", "nnh", "no", "nus", "nyn", "om", "om_KE", "or", "os", "os_RU",
    "pa", "pa_Arab", "pa_Guru", "pcm", "pl", "ps", "ps_PK", "pt",
    "pt_AO", "pt_CH", "pt_CV", "pt_GQ", "pt_GW", "pt_LU", "pt_MO",
    "pt_MZ", "pt_PT", "pt_ST", "pt_TL", "qu", "qu_BO", "qu_EC", "rm",
    "rn", "ro", "ro_MD", "rof", "ru", "ru_BY", "ru_KG", "ru_KZ",
    "ru_MD", "ru_UA", "rw", "rwk", "sa", "sah", "saq", "sat", "sat_Olck",
    "sbp", "sc", "sd", "sd_Arab", "sd_Deva", "se", "se_FI", "se_SE",
    "seh", "ses", "sg", "shi", "shi_Latn", "shi_Tfng", "si", "sk",
    "sl", "smn", "sn", "so", "so_DJ", "so_ET", "so_KE", "sq", "sq_MK",
    "sq_XK", "sr", "sr_Cyrl", "sr_Cyrl_BA", "sr_Cyrl_ME", "sr_Cyrl_XK",
    "sr_Latn", "sr_Latn_BA", "sr_Latn_ME", "sr_Latn_XK", "su", "su_Latn",
    "sv", "sv_AX", "sv_FI", "sw", "sw_CD", "sw_KE", "sw_UG", "ta",
    "ta_LK", "ta_MY", "ta_SG", "te", "teo", "teo_KE", "tg", "th",
    "ti", "ti_ER", "tk", "to", "tr", "tr_CY", "tt", "twq", "tzm",
    "ug", "uk", "und", "ur", "ur_IN", "uz", "uz_Arab", "uz_Cyrl",
    "uz_Latn", "vai", "vai_Latn", "vai_Vaii", "vi", "vun", "wae",
    "wo", "xh", "xog", "yav", "yi", "yo", "yo_BJ", "yrl", "yrl_CO",
    "yrl_VE", "yue", "yue_Hans", "yue_Hant", "zgh", "zh", "zh_Hans",
    "zh_Hans_HK", "zh_Hans_MO", "zh_Hans_SG", "zh_Hant", "zh_Hant_HK",
    "zh_Hant_MO", "zu"
  )
}

#' A list of all supported locales
#'
#' @description
#' The `fdt_locales_lst` object is a list of all supported locales. This is
#' useful when used with the [fdt()] function as the list can be auto-completed
#' with a locale identifier and this generates valid input for the `locale`
#' argument.
#'
#' @return A list where each element corresponds to a supported locale ID.
#'
#' @section Examples:
#'
#' The `fdt_locales_lst` object can be incredibly useful when choosing one of
#' supported locales. You can avoid typing errors and every element of the list
#' is meant to work. In this example, we'll use the `"da"` locale through
#' use of the list.
#'
#' ```r
#' fdt(
#'   input = "2018-07-04 22:05",
#'   format = "yy-MMMM-d",
#'   locale = fdt_locales_lst$da
#' )
#' ```
#' ```
#' #> [1] "18-juli-4"
#' ````
#'
#' @export
fdt_locales_lst <-
  stats::setNames(as.list(fdt_locales_vec()), as.list(fdt_locales_vec()))
