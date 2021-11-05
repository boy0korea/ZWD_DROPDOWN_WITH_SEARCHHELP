"Name: \PR:/1WDA/C5STANDARD==============CP\TY:CL_TOOLBAR_DROPDOWN_BY_KEY\IN:IFUR_NW7__CONTROL\ME:CALL_VISITOR\SE:BEGIN\EI
ENHANCEMENT 0 ZE_WD_DD_SH_C5.
*
    IF zmo_sh IS BOUND AND
       ifur_nw7_toolbarcombobox~enabled EQ abap_true AND
       ifur_nw7_toolbarcombobox~readonly EQ abap_false AND
       mv_displayonly EQ abap_false AND
       ( ifur_nw7_toolbarcombobox~visibility = ifur_nw7=>visibility_visible or ifur_nw7_toolbarcombobox~visibility = ifur_nw7=>visibility_on_demand ).

      zcl_wd_dd_sh=>enh_call_visitor_tb(
        EXPORTING
          visitor   = visitor
          parameter = parameter
          io_me     = me
          io_sh     = cast #( zmo_sh )
      ).
      RETURN.
    ENDIF.
ENDENHANCEMENT.
