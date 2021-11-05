"Name: \PR:/1WDA/C5STANDARD==============CP\TY:CL_TOOLBAR_DROPDOWN_BY_KEY\ME:FINALIZE\SE:BEGIN\EI
ENHANCEMENT 0 ZE_WD_DD_SH_C5.
*
    IF zmo_sh IS BOUND.
      zcl_wd_dd_sh=>enh_finalize_tb(
        CHANGING
          co_sh = zmo_sh
      ).
    ENDIF.
ENDENHANCEMENT.
