CLASS zcl_wd_dd_sh DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-DATA gv_on TYPE flag .

    CLASS-METHODS class_constructor .
    CLASS-METHODS enh_set_content
      IMPORTING
        !io_view_element      TYPE REF TO cl_wdr_view_element
      RETURNING
        VALUE(ro_input_field) TYPE REF TO cl_wd_input_field .
    CLASS-METHODS enh_call_visitor
      IMPORTING
        !visitor   TYPE REF TO ifur_nw7_visitor
        !parameter TYPE REF TO data
        !io_me     TYPE REF TO cl_nw7_view_element_adapter
        !io_sh     TYPE REF TO cl_nw7_view_element_adapter .
    CLASS-METHODS enh_finalize
      CHANGING
        !co_sh TYPE REF TO /1wda/dtable_cell_editor .
    CLASS-METHODS enh_handle_event
      IMPORTING
        !m_view_element    TYPE REF TO cl_wdr_view_element
        !m_context_element TYPE REF TO if_wd_context_element .
    CLASS-METHODS enh_set_content_tb
      IMPORTING
        !io_view_element      TYPE REF TO cl_wdr_view_element
      RETURNING
        VALUE(ro_input_field) TYPE REF TO cl_wd_toolbar_input_field .
    CLASS-METHODS enh_call_visitor_tb
      IMPORTING
        !visitor   TYPE REF TO ifur_nw7_visitor
        !parameter TYPE REF TO data
        !io_me     TYPE REF TO ifur_nw7_toolbarcombobox
        !io_sh     TYPE REF TO ifur_nw7_toolbarinputfield .
    CLASS-METHODS enh_finalize_tb
      CHANGING
        !co_sh TYPE REF TO /1wda/dtable_cell_editor .
    CLASS-METHODS enh_handle_event_tb
      IMPORTING
        !m_view_element    TYPE REF TO cl_wdr_view_element
        !m_context_element TYPE REF TO if_wd_context_element .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_WD_DD_SH IMPLEMENTATION.


  METHOD class_constructor.
    CHECK: wdr_task=>application IS NOT INITIAL.

    " SET/GET Parameter
    GET PARAMETER ID 'ZWD_DD_SH' FIELD gv_on.
    CHECK: gv_on IS INITIAL.

    " URL param
    READ TABLE wdr_task=>client_window->parameters INTO DATA(ls_param) WITH KEY name = 'zwd_dd_sh'.
    IF sy-subrc EQ 0.
      gv_on = ls_param-value.
      RETURN.
    ENDIF.

    " Appl. param
    gv_on = wdr_task=>application->application_info->get_property( 'ZWD_DD_SH' ).

  ENDMETHOD.


  METHOD enh_call_visitor.
    DATA: lr_html_dd TYPE REF TO string,
          lr_html_sh TYPE REF TO string,
          lv_width   TYPE string.
    FIELD-SYMBOLS: <lv_html>    TYPE string,
                   <lv_html_dd> TYPE string,
                   <lv_html_sh> TYPE string.

*    visitor->combobox( control = me  parameter = parameter ).

    ASSIGN parameter->* TO <lv_html>.

    CREATE DATA lr_html_dd.
    ASSIGN lr_html_dd->* TO <lv_html_dd>.
    visitor->combobox(
      EXPORTING
        control   = CAST #( io_me )
        parameter = lr_html_dd
    ).
*    IFUR_NW7_GRIDLAYOUTCELL~WIDTH
    IF ( io_me->m_parent IS INSTANCE OF ifur_nw7_gridlayoutcell AND CAST ifur_nw7_gridlayoutcell( io_me->m_parent )->width IS NOT INITIAL ) OR
       ( io_me->m_parent IS INSTANCE OF ifur_nw7_matrixlayoutcell AND CAST ifur_nw7_matrixlayoutcell( io_me->m_parent )->width IS NOT INITIAL ) OR
       ( io_me->m_parent IS INSTANCE OF ifur_nw7_rasterlayoutitem AND CAST ifur_nw7_rasterlayoutitem( io_me->m_parent )->width IS NOT INITIAL ).
      lv_width = `width="100%"`.
    ENDIF.

    CREATE DATA lr_html_sh.
    ASSIGN lr_html_sh->* TO <lv_html_sh>.
    visitor->inputfield(
      EXPORTING
        control   = CAST #( io_sh )
        parameter = lr_html_sh
    ).
    REPLACE `type="text"` IN <lv_html_sh> WITH `type="hidden"`.

    <lv_html> = <lv_html>
             && `<table ` && lv_width && ` cellpadding="0" cellspacing="0"><tr><td>`
             && <lv_html_dd>
             && `</td><td width="22px">`
             && <lv_html_sh>
             && `</td></tr></table>`.

  ENDMETHOD.


  METHOD enh_call_visitor_tb.
    DATA: lr_html_dd TYPE REF TO string,
          lr_html_sh TYPE REF TO string.
    FIELD-SYMBOLS: <lv_html>    TYPE string,
                   <lv_html_dd> TYPE string,
                   <lv_html_sh> TYPE string.

*    visitor->combobox( control = me  parameter = parameter ).

    ASSIGN parameter->* TO <lv_html>.

    CREATE DATA lr_html_dd.
    ASSIGN lr_html_dd->* TO <lv_html_dd>.
    visitor->toolbarcombobox(
      EXPORTING
        control   = io_me
        parameter = lr_html_dd
    ).
    REPLACE `><` IN <lv_html_dd> WITH ` style="padding-right: 0px;" ><`.

    CREATE DATA lr_html_sh.
    ASSIGN lr_html_sh->* TO <lv_html_sh>.
    visitor->toolbarinputfield(
      EXPORTING
        control   = io_sh
        parameter = lr_html_sh
    ).
    REPLACE `type="text"` IN <lv_html_sh> WITH `type="hidden"`.
    REPLACE `style="` IN <lv_html_sh> WITH `style="padding-left: 0px;`.

    <lv_html> = <lv_html>
             && `<table cellpadding="0" cellspacing="0"><tr><td>`
             && <lv_html_dd>
             && `</td><td width="22px">`
             && <lv_html_sh>
             && `</td></tr></table>`.

  ENDMETHOD.


  METHOD enh_finalize.
    IF co_sh IS BOUND.
      co_sh->finalize( ).
      CLEAR co_sh.
    ENDIF.
  ENDMETHOD.


  METHOD enh_finalize_tb.
    IF co_sh IS BOUND.
      co_sh->finalize( ).
      CLEAR co_sh.
    ENDIF.
  ENDMETHOD.


  METHOD enh_handle_event.
    DATA: lo_ddk       TYPE REF TO cl_wd_dropdown_by_key,
          lv_on_select TYPE string.
    lo_ddk ?= m_view_element.
    lv_on_select = lo_ddk->get_on_select( ).
    CHECK: lv_on_select IS NOT INITIAL.


    DATA l_server_event TYPE REF TO cl_wdr_server_event.    "#EC NEEDED
    CREATE OBJECT l_server_event
      EXPORTING
        source           = lo_ddk
        command          = lv_on_select
        type             = if_wd_event=>co_action_event
        view_name        = lo_ddk->_component->id && `.` && lo_ddk->view->name "#EC NOTEXT
        ui_element_id    = lo_ddk->id
        ui_element_event = 'ON_SELECT'. "#EC NOTEXT
    DATA l_event_param_mappings LIKE cl_wdr_event=>parameters. "#EC NEEDED
    lo_ddk->mapped_on_select( IMPORTING parameters = l_event_param_mappings ).
    l_server_event->add_parameters( l_event_param_mappings ).
    l_server_event->set_parameter( name = 'ID'  value = lo_ddk->id ). "#EC NOTEXT
    l_server_event->set_parameter( name = 'CONTEXT_ELEMENT'  object = m_context_element type = cl_abap_typedescr=>typekind_oref ). "#EC NOTEXT

*    i_event_queue->add_event( l_server_event ).
    cl_wdr_value_help_handler=>set_event(
      EXPORTING
        view  = lo_ddk->view
        event = l_server_event
    ).
  ENDMETHOD.


  METHOD enh_handle_event_tb.
    DATA: lo_ddk       TYPE REF TO cl_wd_toolbar_dropdown_by_key,
          lv_on_select TYPE string.
    lo_ddk ?= m_view_element.
    lv_on_select = lo_ddk->get_on_select( ).
    CHECK: lv_on_select IS NOT INITIAL.


    DATA l_server_event TYPE REF TO cl_wdr_server_event.    "#EC NEEDED
    CREATE OBJECT l_server_event
      EXPORTING
        source           = lo_ddk
        command          = lv_on_select
        type             = if_wd_event=>co_action_event
        view_name        = lo_ddk->_component->id && `.` && lo_ddk->view->name "#EC NOTEXT
        ui_element_id    = lo_ddk->id
        ui_element_event = 'ON_SELECT'. "#EC NOTEXT
    DATA l_event_param_mappings LIKE cl_wdr_event=>parameters. "#EC NEEDED
    lo_ddk->mapped_on_select( IMPORTING parameters = l_event_param_mappings ).
    l_server_event->add_parameters( l_event_param_mappings ).
    l_server_event->set_parameter( name = 'ID'  value = lo_ddk->id ). "#EC NOTEXT
    l_server_event->set_parameter( name = 'CONTEXT_ELEMENT'  object = m_context_element type = cl_abap_typedescr=>typekind_oref ). "#EC NOTEXT

*    i_event_queue->add_event( l_server_event ).
    cl_wdr_value_help_handler=>set_event(
      EXPORTING
        view  = lo_ddk->view
        event = l_server_event
    ).
  ENDMETHOD.


  METHOD enh_set_content.
    DATA: lo_ddk TYPE REF TO cl_wd_dropdown_by_key.

    CHECK: gv_on EQ abap_true.

    lo_ddk ?= io_view_element.

    cl_wd_input_field=>new_input_field(
      EXPORTING
        bind_value                  = lo_ddk->bound_selected_key( )
*        id                          = '__' && lo_ddk->id
        no_history                  = abap_true
*        on_enter                    = lo_ddk->get_on_select( )
        view                        = lo_ddk->view
        width                       = '22px'
      RECEIVING
        control                     = ro_input_field
    ).

  ENDMETHOD.


  METHOD enh_set_content_tb.
    DATA: lo_ddk TYPE REF TO cl_wd_toolbar_dropdown_by_key.

    CHECK: gv_on EQ abap_true.

    lo_ddk ?= io_view_element.

    cl_wd_toolbar_input_field=>new_toolbar_input_field(
      EXPORTING
        bind_value                  = lo_ddk->bound_selected_key( )
*        id                          = '__' && lo_ddk->id
        no_history                  = abap_true
*        on_enter                    = lo_ddk->get_on_select( )
        view                        = lo_ddk->view
        width                       = '22px'
      RECEIVING
        control                     = ro_input_field
    ).

  ENDMETHOD.
ENDCLASS.
