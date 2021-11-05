"Name: \PR:/1WDA/C0STANDARD==============CP\TY:CL_DROPDOWN_BY_KEY\ME:FINALIZE\SE:BEGIN\EI
ENHANCEMENT 0 ZE_WD_DD_SH_C0.
*
    IF zmo_sh IS BOUND.
      zcl_wd_dd_sh=>enh_finalize(
        CHANGING
          co_sh = zmo_sh
      ).
    ENDIF.
ENDENHANCEMENT.
