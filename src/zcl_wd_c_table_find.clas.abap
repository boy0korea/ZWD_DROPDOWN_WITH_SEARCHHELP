CLASS zcl_wd_c_table_find DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_search_state,
        not_active     TYPE string        VALUE 'NOT_ACTIVE', " no search currently active
        active         TYPE string            VALUE 'ACTIVE', " means search is executed and search UI is visible
        to_be_executed TYPE string    VALUE 'TO_BE_EXECUTED', " search button has been just pressed
        jump_to_next   TYPE string      VALUE 'JUMP_TO_NEXT', " means jump to next
        jump_to_prev   TYPE string      VALUE 'JUMP_TO_PREV', " means jump to prev
      END OF gc_search_state .

    CLASS-METHODS create
      IMPORTING
        !io_view_api TYPE REF TO if_wd_view_controller
        !io_c_table  TYPE REF TO cl_wd_c_table OPTIONAL .
    METHODS constructor
      IMPORTING
        !io_c_table TYPE REF TO cl_wd_c_table .
    METHODS get_search_string
      RETURNING
        VALUE(rv_search_string) TYPE string .
    METHODS execute_search .
    METHODS jump_to_next_hit .
    METHODS jump_to_prev_hit .
    METHODS deactivate .
    METHODS get_search_state
      RETURNING
        VALUE(rv_state) TYPE string .
    METHODS set_search_state
      IMPORTING
        !iv_search_state TYPE string .
    METHODS set_user_changed_data .
    METHODS on_do_search .
    METHODS on_search_jump_to_next_hit
      IMPORTING
        !iv_search_direction_next TYPE abap_bool .
    CLASS-METHODS get_instance
      IMPORTING
        !io_c_table        TYPE REF TO cl_wd_c_table
      RETURNING
        VALUE(ro_instance) TYPE REF TO zcl_wd_c_table_find .
    METHODS on_delete
        FOR EVENT component_deleted OF cl_wdr_component_usage .
private section.

  types:
    BEGIN OF ty_s_instance,
        c_table  TYPE REF TO cl_wd_c_table,
        instance TYPE REF TO zcl_wd_c_table_find,
      END OF ty_s_instance .
  types:
    ty_t_instance TYPE HASHED TABLE OF ty_s_instance WITH UNIQUE KEY c_table .
  types:
    BEGIN OF ty_s_wd_usage,
        view  TYPE REF TO if_wd_view_controller,
        usage TYPE REF TO cl_wdr_component_usage,
      END OF ty_s_wd_usage .
  types:
    ty_t_wd_usage TYPE HASHED TABLE OF ty_s_wd_usage WITH UNIQUE KEY view .
  types:
    BEGIN OF ty_s_column_mapping,
        ui_column         TYPE REF TO cl_wd_c_table_column,
        ui_cell_editor_id TYPE string,
        table_field_name  TYPE string,
      END OF ty_s_column_mapping .
  types:
    ty_t_column_mapping TYPE STANDARD TABLE OF ty_s_column_mapping .

  class-data GT_INSTANCE type TY_T_INSTANCE .
  class-data GT_WD_USAGE type TY_T_WD_USAGE .
  constants:
    BEGIN OF gc_scroll_type,
        jump_to_next        TYPE string VALUE 'JUMP_TO_NEXT', " means a node has been expanded/collapsed
        jump_to_prev        TYPE string VALUE 'JUMP_TO_PREV', " means a node has been expanded/collapsed
        jump_to_initial_hit TYPE string VALUE 'JUMP_TO_INITIAL_HIT', " means a node has been expanded/collapsed
      END OF gc_scroll_type .
  data MV_SEARCH_STATE type STRING .       " see gc_search_state
  data MV_USER_CHANGED_DATA type ABAP_BOOL .
  data MO_TOOLBOX type ref to CL_SALV_STI_TOOLBOX .
  data MV_SEARCH_STRING type STRING .
  data MV_SEARCH_STRING_PREV type STRING .
  data MT_TOOLBOX_HITS type CL_SALV_STI_TOOLBOX=>YT_HIT .
  data MV_CURRENT_HIT_IDX type I .
  data MV_AUX_INDEX type I .
  data MV_UPDATE_FRONTEND_NECESSARY type ABAP_BOOL .
  data MO_RTTI type ref to CL_ABAP_STRUCTDESCR .
  data MV_IS_RTTI_STATIC type FLAG .
  data MO_C_TABLE type ref to CL_WD_C_TABLE .
  data MO_VIEW type ref to IF_WD_VIEW .
  data MO_CONTEXT_ROOT type ref to IF_WD_CONTEXT_NODE .
  data MO_PARENT_USAGE type ref to CL_WDR_COMPONENT_USAGE .
  data MT_COLUMN_MAPPING type TY_T_COLUMN_MAPPING .
  data:
    mt_cell_editor_id TYPE TABLE OF string .

  class-methods GET_WD_USAGE
    importing
      !IO_VIEW_API type ref to IF_WD_VIEW_CONTROLLER
    returning
      value(RO_WD_USAGE) type ref to CL_WDR_COMPONENT_USAGE .
  methods CREATE_MT_COLUMN_MAPPING .
  methods GET_FIRST_VISI_ROW
    returning
      value(RV_ROW_INDEX) type I .
  methods GET_LAST_VISI_ROW
    returning
      value(RV_ROW_INDEX) type I .
  methods UPDATE_FIRST_VISI_ROW
    importing
      !IR_CURRENT_HIT type ref to CL_SALV_STI_TOOLBOX=>YS_HIT
      !IV_SCROLL_TYPE type STRING .
  methods DETERMINE_FIRST_HIT .
  methods DETERMINE_NEXT_HIT
    importing
      !IV_RELATIVE_TO_ROW type I optional
    returning
      value(RV_START_OVER) type ABAP_BOOL .
  methods DETERMINE_PREV_HIT
    importing
      !IV_RELATIVE_TO_ROW type I optional
    returning
      value(RV_START_OVER) type ABAP_BOOL .
  methods GET_REF_OF_CURRENT_HIT
    returning
      value(RR_CURRENT_HIT) type ref to CL_SALV_STI_TOOLBOX=>YS_HIT .
  methods TRIGGER_SEARCH .
  methods UPDATE_FRONTEND
    importing
      !IR_CURRENT_HIT type ref to CL_SALV_STI_TOOLBOX=>YS_HIT optional
      !IV_SET_TEXT_MARKERS type ABAP_BOOL .
  methods CLEAR_FRONT_END .
  methods GET_TEXT_MARKERS
    importing
      !IR_CURRENT_HIT type ref to CL_SALV_STI_TOOLBOX=>YS_HIT
    exporting
      !ET_TEXT_MARKERS type WDUI_C_TABLE_TEXT_MARKERS .
  methods CONFIGURE_COLUMNS
    importing
      !IO_COLUMN_CATALOG type ref to IF_SALV_COLUMN_CATALOG .
  methods UPDATE_MATCHES_STRING .
  methods CREATE_DYNAMIC_NODE .
  methods RENDER_SEARCH_AREA .
  methods GET_NEW_SEARCH_STRING
    returning
      value(RV_SEARCH_STRING) type STRING .
  methods CREATE_MO_RTTI .
ENDCLASS.



CLASS ZCL_WD_C_TABLE_FIND IMPLEMENTATION.


  METHOD update_matches_string.

    DATA: lo_nd           TYPE REF TO if_wd_context_node,
          lo_el           TYPE REF TO if_wd_context_element,
          lv_text         TYPE string,
          lv_nr_as_string TYPE string,
          lv_hits         TYPE i.

    lo_nd = mo_context_root->get_child_node( if_fpm_guibb_constants=>gc_guibb_list_nodes-dynamic ).
*    lo_nd = lo_nd->get_child_node( if_fpm_guibb_constants=>gc_guibb_list_nodes-table_properties ).
    lo_nd = lo_nd->get_child_node( mo_c_table->id ).
    lo_el = lo_nd->get_element( ).

    IF mv_search_state = gc_search_state-active.
      lv_hits = lines( mt_toolbox_hits ).
      IF mv_user_changed_data = abap_true.
        lv_text = TEXT-007.
        lv_nr_as_string = lv_hits.
        REPLACE '&1' WITH lv_nr_as_string INTO lv_text.
      ELSE.
        CASE lv_hits.
          WHEN 0.
            lv_text = TEXT-007.
            lv_nr_as_string = lv_hits.
            REPLACE '&1' WITH lv_nr_as_string INTO lv_text.
          WHEN 1.
            lv_text = TEXT-008.
            lv_nr_as_string = lv_hits.
            REPLACE '&1' WITH lv_nr_as_string INTO lv_text.
          WHEN OTHERS.
            lv_text = TEXT-006.
            lv_nr_as_string = lv_hits.
            REPLACE '&2' WITH lv_nr_as_string INTO lv_text.
            lv_nr_as_string = mv_current_hit_idx.
            REPLACE '&1' WITH lv_nr_as_string INTO lv_text.
        ENDCASE.
      ENDIF.
    ENDIF.

    lo_el->set_attribute( EXPORTING value = lv_text
                                    name  = if_fpm_list_types=>cs_list_attribute_internal-search_text_matches ).
    " visibility
    IF lv_text IS INITIAL.
      lo_el->set_attribute( EXPORTING value = cl_wd_uielement=>e_visible-none
                                      name  = if_fpm_list_types=>cs_list_attribute_internal-search_text_matches_visible ).
    ELSE.
      lo_el->set_attribute( EXPORTING value = cl_wd_uielement=>e_visible-visible
                                      name  = if_fpm_list_types=>cs_list_attribute_internal-search_text_matches_visible ).
    ENDIF.
  ENDMETHOD.


  METHOD update_frontend.
    DATA: lv_index TYPE i.

    " take care of focus
    IF ir_current_hit IS SUPPLIED.
      mo_view->request_focus_on_view_elem(
        EXPORTING
          view_element    = mo_view->get_element( mt_cell_editor_id[ ir_current_hit->column_idx ] )
          context_element = mo_c_table->get_data_source( )->get_element( index = ir_current_hit->row )
      ).
    ENDIF.

    " take care of wd abap text markers
    IF iv_set_text_markers = abap_true OR mv_update_frontend_necessary = abap_true.
      DATA: lt_text_markers TYPE wdui_c_table_text_markers.
      get_text_markers( EXPORTING ir_current_hit = ir_current_hit
                        IMPORTING et_text_markers = lt_text_markers ).
      mo_c_table->set_text_markers( lt_text_markers ).
    ENDIF.
    mv_update_frontend_necessary = abap_false.
  ENDMETHOD.


  METHOD update_first_visi_row.
    DATA: lv_last_visi_row           TYPE i,
          lv_first_visi_row          TYPE i,
          lv_frontend_idx_of_current TYPE i,
          lv_new_first_visi_row      TYPE i.

    CASE iv_scroll_type.
        " -------------------------------------------------------------
      WHEN zcl_wd_c_table_find=>gc_scroll_type-jump_to_next.
        " -------------------------------------------------------------
        lv_last_visi_row = get_last_visi_row( ).
        IF ir_current_hit->row <= ( lv_last_visi_row - 1 ).
          " means: the current hit is still in the visible area -> no scrolling necessary
        ELSE.
          " scrolling is necessary
          lv_new_first_visi_row = ir_current_hit->row - mo_c_table->get_visible_row_count( ) + 1.
          mo_c_table->set_first_visible_row( value = lv_new_first_visi_row + 1 ).
        ENDIF.
        " -------------------------------------------------------------
      WHEN zcl_wd_c_table_find=>gc_scroll_type-jump_to_prev.
        " -------------------------------------------------------------
        lv_first_visi_row = get_first_visi_row( ).
        IF ir_current_hit->row >= ( lv_first_visi_row + 1 ).
          " means: the current hit is still in the visible area -> no scrolling necessary
        ELSE.
          " scrolling is necessary
          lv_new_first_visi_row = ir_current_hit->row.
          mo_c_table->set_first_visible_row( value = lv_new_first_visi_row - 1 ).
        ENDIF.
        " -------------------------------------------------------------
      WHEN zcl_wd_c_table_find=>gc_scroll_type-jump_to_initial_hit.
        " -------------------------------------------------------------
        " get upper boundary
        lv_first_visi_row = get_first_visi_row( ).
        " get lower boundary
        lv_last_visi_row = get_last_visi_row( ).
        " check boundaries
        IF ir_current_hit->row < lv_first_visi_row.
          " scrolling is necessary
          lv_new_first_visi_row = ir_current_hit->row.
          mo_c_table->set_first_visible_row( value = lv_new_first_visi_row ).
        ENDIF.
        IF ir_current_hit->row > lv_last_visi_row.
          " scrolling is necessary
          lv_new_first_visi_row = ir_current_hit->row - mo_c_table->get_visible_row_count( ) + 1.
          mo_c_table->set_first_visible_row( value = lv_new_first_visi_row ).
        ENDIF.
    ENDCASE.
  ENDMETHOD.


  METHOD trigger_search.
    " call ats toolbox
    DATA: ls_column_mapping TYPE ty_s_column_mapping,
          lo_table_type     TYPE REF TO cl_abap_tabledescr,
          lr_data           TYPE REF TO data,
          lt_ats_columns    TYPE string_table,
          lt_el             TYPE wdr_context_element_set,
          lo_el             TYPE REF TO if_wd_context_element.
    FIELD-SYMBOLS: <lt_data> TYPE table,
                   <ls_data> TYPE data,
                   <lv_data> TYPE data.

    CLEAR: mt_cell_editor_id.

    " now get the columns that are relevant to search in
    LOOP AT mt_column_mapping INTO ls_column_mapping.
      CHECK: ls_column_mapping-ui_column->get_visible( ) EQ if_fpm_constants=>gc_visibility-visible.
      APPEND ls_column_mapping-table_field_name TO lt_ats_columns.
      APPEND ls_column_mapping-ui_cell_editor_id TO mt_cell_editor_id.
    ENDLOOP.

    lo_table_type = cl_abap_tabledescr=>create( mo_rtti ).
    CREATE DATA lr_data TYPE HANDLE lo_table_type.
    ASSIGN lr_data->* TO <lt_data>.

    IF mv_is_rtti_static EQ abap_true.
      mo_c_table->get_data_source( )->get_static_attributes_table(
        IMPORTING
          table = <lt_data>
      ).
    ELSE.
      lt_el = mo_c_table->get_data_source( )->get_elements( ).
      LOOP AT lt_el INTO lo_el.
        APPEND INITIAL LINE TO <lt_data> ASSIGNING <ls_data>.
        LOOP AT mt_column_mapping INTO ls_column_mapping.
          ASSIGN COMPONENT ls_column_mapping-table_field_name OF STRUCTURE <ls_data> TO <lv_data>.
          CHECK: sy-subrc EQ 0.
          lo_el->get_attribute(
            EXPORTING
              name  = ls_column_mapping-table_field_name
            IMPORTING
              value = <lv_data>
          ).
        ENDLOOP.
      ENDLOOP.
    ENDIF.

    TRY.
        CLEAR: mt_toolbox_hits.
        mo_toolbox->find( EXPORTING i_search_term     = mv_search_string
                                    it_search_columns = lt_ats_columns
                                    it_data           = <lt_data>
                          IMPORTING et_hits           = mt_toolbox_hits ).
      CATCH cx_salv_table_type_changed ##NO_HANDLER.
      CATCH cx_salv_column_unknown ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


  METHOD set_user_changed_data.
    mv_user_changed_data = abap_true.
  ENDMETHOD.


  METHOD set_search_state.
    mv_search_state = iv_search_state.
  ENDMETHOD.


  METHOD render_search_area.
* 필요 ACTION 3개.
*                        on_enter     = 'SEARCH_EXECUTE'
*method onactionsearch_execute .
*
*  wd_assist->on_do_search( ).
*
*endmethod.
*                                  on_action    = 'SEARCH_LEFT'
*method onactionsearch_left.
*
*  wd_assist->on_search_jump_to_next_hit( iv_search_direction_next = abap_false ).
*
*endmethod.
*                                  on_action    = 'SEARCH_RIGHT'
*method onactionsearch_right.
*
*  wd_assist->on_search_jump_to_next_hit( iv_search_direction_next = abap_true ).
*
*endmethod.

    DATA: lv_table_id TYPE string,
          lo_toolbar  TYPE REF TO cl_wd_toolbar.

    lv_table_id = mo_c_table->id.
    lo_toolbar = mo_c_table->get_toolbar( ).
    IF lo_toolbar IS INITIAL.
      lo_toolbar = cl_wd_toolbar=>new_toolbar(
        id       = lv_table_id && `___TOOLBAR`              "#EC NOTEXT
        enabled  = abap_true
        wrapping = abap_true
      ).
      mo_c_table->set_toolbar( lo_toolbar ).
    ENDIF.

*    DATA(lo_search_button) = cl_wd_toolbar_button=>new_toolbar_button(
*      id           = `BTN_FPM_SEARCH`                       "#EC NOTEXT
*      on_action    = `OPEN_SEARCH`                          "#EC NOTEXT
*      design       = cl_wd_toolbar_button=>e_design-standard
*      enabled      = abap_true
*      image_first  = abap_true
*      image_source = `~Icon/Log`                            "#EC NOTEXT
*      tooltip      = |{ TEXT-016 }|
*      hotkey       = cl_wd_link_to_action=>e_hotkey-ctrl_f ).
*    lo_toolbar->add_toolbar_right_item( lo_search_button ).



    " create popin
    DATA: lo_popin        TYPE REF TO cl_wd_popin,
          lv_path         TYPE string,
          lv_path_visible TYPE string.
*    lv_path  =
*            |{ if_fpm_guibb_constants=>gc_guibb_list_nodes-dynamic }.|
**      && |{ if_fpm_guibb_constants=>gc_guibb_list_nodes-table_properties }.|
*      && |{ mo_c_table->id }.|
*      && |{ if_fpm_guibb_constants=>gc_guibb_tree_attributes-search_open }| .
    lo_popin = cl_wd_popin=>new_popin( id = lv_table_id && '___POPIN_SEARCH'
                                       has_content_padding = abap_false ).
*                                       bind_visible = lv_path ).
    lo_toolbar->set_toolbar_popin( the_toolbar_popin = lo_popin ).

    DATA lo_lc TYPE REF TO cl_wd_layout_container.
    lo_lc ?= cl_wd_layout_container=>new_layout_container( width = '100%' ).
    cl_wd_matrix_layout=>new_matrix_layout( EXPORTING container = lo_lc
                                           stretched_horizontally = abap_true
                                           stretched_vertically   = abap_false ).
    cl_wd_flow_data=>new_flow_data( EXPORTING element = lo_lc ).
    lo_popin->set_content( EXPORTING the_content = lo_lc  ).


    DATA lo_tc TYPE REF TO cl_wd_transparent_container.
    lo_tc ?= cl_wd_transparent_container=>new_transparent_container( width = '100%' ).
    cl_wd_flow_layout=>new_flow_layout( EXPORTING container = lo_tc
                                        wrapping = abap_true ).

    cl_wd_matrix_head_data=>new_matrix_head_data( EXPORTING element = lo_tc
                                    h_align = cl_wd_matrix_head_data=>e_h_align-end_of_line
                                    v_align = cl_wd_matrix_head_data=>e_v_align-top   ).

    lo_lc->add_child( lo_tc ).

    lv_path  =
            |{ if_fpm_guibb_constants=>gc_guibb_list_nodes-dynamic }.|
*      && |{ if_fpm_guibb_constants=>gc_guibb_list_nodes-table_properties }.|
      && |{ mo_c_table->id }.|
      && |{ if_fpm_guibb_constants=>gc_guibb_tree_attributes-search_string }|.
    DATA lo_label TYPE REF TO cl_wd_label.
    lo_label ?= cl_wd_label=>new_label(
*                        id           = 'FPM_SEARCH_STRING_LABEL'
                        editable     = abap_false
                        text         = |{ TEXT-012 }|
                        label_for    = lv_table_id && '___SEARCH_STRING'
                        wrapping     = abap_true ).
    DATA lo_inp_field TYPE REF TO cl_wd_input_field.
    lo_inp_field ?= cl_wd_input_field=>new_input_field(
                        id           = lv_table_id && '___SEARCH_STRING'
                        bind_value = lv_path
                        input_help = '03' "search
                        " input_prompt = |{ text-012 }|
                        on_enter     = 'ZWD_C_TABLE_FIND_SEARCH_EXECUTE'
                        width        = '12em' ).

    DATA lo_txt_field TYPE REF TO cl_wd_text_view.
    lv_path  = |{ if_fpm_guibb_constants=>gc_guibb_list_nodes-dynamic }.|
*            && |{ if_fpm_guibb_constants=>gc_guibb_list_nodes-table_properties }.|
            && |{ mo_c_table->id }.|
            && |{ if_fpm_list_types=>cs_list_attribute_internal-search_text_matches }|.
    lv_path_visible  = |{ if_fpm_guibb_constants=>gc_guibb_list_nodes-dynamic }.|
*            && |{ if_fpm_guibb_constants=>gc_guibb_list_nodes-table_properties }.|
            && |{ mo_c_table->id }.|
            && |{ if_fpm_list_types=>cs_list_attribute_internal-search_text_matches_visible }|.
    lo_txt_field ?= cl_wd_text_view=>new_text_view(
                                                    id           = lv_table_id && '___SEARCH_MATCHES'
                                                    bind_visible = lv_path_visible
                                                    bind_text    = lv_path
                                                    wrapping     = abap_false ).

    cl_wd_flow_data=>new_flow_data( EXPORTING element = lo_inp_field
                                              cell_design = cl_wd_flow_data=>e_cell_design-l_pad ).
    cl_wd_flow_data=>new_flow_data( EXPORTING element = lo_label
                                             cell_design = cl_wd_flow_data=>e_cell_design-l_pad
                                             v_gutter = cl_wd_matrix_data=>e_v_gutter-medium ).
    cl_wd_flow_data=>new_flow_data( EXPORTING element = lo_txt_field
                                             cell_design = cl_wd_flow_data=>e_cell_design-l_pad ).

    lo_tc->add_child( lo_txt_field ).
    lo_tc->add_child( lo_label ).
    lo_tc->add_child( lo_inp_field ).

    DATA lo_btn_tc TYPE REF TO cl_wd_transparent_container.
    lo_btn_tc ?= cl_wd_transparent_container=>new_transparent_container(  ).
    cl_wd_flow_layout=>new_flow_layout( EXPORTING container = lo_btn_tc
                                                  wrapping = abap_true ).
*    IF mo_config_data->get_table_settings( )-allow_search NE 'X' AND
*       mo_config_data->get_table_settings( )-allow_search NE 'Y'.
    DATA lo_lnk_tc TYPE REF TO cl_wd_link_to_action.
    lo_lnk_tc ?= cl_wd_link_to_action=>new_link_to_action(
                                  id           = lv_table_id && '___SEARCH_LEFT'
                                  image_source = '~Icon/MoveUp'
                                  tooltip      = |{ TEXT-013 }|
                                  on_action    = 'ZWD_C_TABLE_FIND_SEARCH_LEFT'
                                  hotkey       = cl_wd_link_to_action=>e_hotkey-alt_arrow_up ).
    cl_wd_flow_data=>new_flow_data( EXPORTING element = lo_lnk_tc
                                   cell_design = cl_wd_flow_data=>e_cell_design-l_pad ).
    lo_tc->add_child( lo_lnk_tc ).

    lo_lnk_tc ?= cl_wd_link_to_action=>new_link_to_action(
                                  id           = lv_table_id && '___SEARCH_RIGHT'
                                  image_source = '~Icon/MoveDown'
                                  tooltip      = |{ TEXT-014 }|
                                  on_action    = 'ZWD_C_TABLE_FIND_SEARCH_RIGHT'
                                  hotkey       = cl_wd_link_to_action=>e_hotkey-alt_arrow_down ).
    cl_wd_flow_data=>new_flow_data( EXPORTING element = lo_lnk_tc
                                  cell_design = cl_wd_flow_data=>e_cell_design-l_pad ).
    lo_tc->add_child( lo_lnk_tc ).
*    ENDIF.

*    lo_lnk_tc ?= cl_wd_link_to_action=>new_link_to_action(
*                                  id           = 'FPM_SEARCH_CLOSE'
*                                  image_source = '~Icon/Cancel'
*                                  tooltip      = |{ TEXT-015 }|
*                                  on_action    = 'SEARCH_CLOSE'
*                                  hotkey       =  cl_wd_link_to_action=>e_hotkey-ctrl_shift_f ).  " '70'   ).   " ctrl-shift-f  "cl_wd_link_to_action=>e_hotkey-ctrl_shift_f
*    cl_wd_flow_data=>new_flow_data( EXPORTING element = lo_lnk_tc
*                                    cell_design = cl_wd_flow_data=>e_cell_design-l_pad  ).
*    cl_wd_flow_data=>new_flow_data( EXPORTING element = lo_btn_tc
*                                    cell_design = cl_wd_flow_data=>e_cell_design-l_pad  ).
*    lo_tc->add_child( lo_lnk_tc ).

  ENDMETHOD.


  METHOD on_search_jump_to_next_hit.

    IF get_search_state( ) = gc_search_state-active AND
       get_new_search_string( ) = get_search_string( ).
      IF iv_search_direction_next = abap_true.
        jump_to_next_hit( ).
      ELSE.
        jump_to_prev_hit( ).
      ENDIF.
      execute_search( ).
    ELSE.
      on_do_search( ).
    ENDIF.

  ENDMETHOD.


  METHOD on_do_search.
    " ----------------------------------------------------------------
    " give the string to the search tool
    " ----------------------------------------------------------------
    set_search_state( gc_search_state-to_be_executed ).

    execute_search( ).
  ENDMETHOD.


  METHOD on_delete.
    DATA: ls_wd_usage TYPE ty_s_wd_usage.

    DELETE gt_instance WHERE c_table = mo_c_table.
    DELETE gt_wd_usage WHERE view = mo_view.

    IF mo_parent_usage IS NOT INITIAL.
      SET HANDLER on_delete FOR mo_parent_usage ACTIVATION abap_false.
    ENDIF.

    CLEAR: mo_c_table, mo_context_root, mo_parent_usage, mo_rtti, mo_toolbox, mo_view, mt_cell_editor_id, mt_column_mapping, mt_toolbox_hits.
  ENDMETHOD.


  METHOD jump_to_prev_hit.
    CHECK mv_search_state = gc_search_state-active.
    mv_search_state = gc_search_state-jump_to_prev.
    mv_update_frontend_necessary = abap_true.
  ENDMETHOD.


  METHOD jump_to_next_hit.
    CHECK mv_search_state = gc_search_state-active.
    mv_search_state = gc_search_state-jump_to_next.
    mv_update_frontend_necessary = abap_true.
  ENDMETHOD.


  METHOD get_wd_usage.
    DATA: ls_wd_usage TYPE ty_s_wd_usage,
          lv_comp     TYPE string,
          lv_usage    TYPE string,
          lo_f        TYPE REF TO ziwci_wd_c_table_find.

    READ TABLE gt_wd_usage INTO ls_wd_usage WITH KEY view = io_view_api.
    IF sy-subrc <> 0.
      ls_wd_usage-view = io_view_api.
      lv_comp = 'ZWD_C_TABLE_FIND'.
      lv_usage = 'ZWD_C_TABLE_FIND_' && io_view_api->name.
      ls_wd_usage-usage ?= cl_wdr_runtime_services=>get_component_usage(
                             component            = io_view_api->get_component( )
                             used_component_name  = lv_comp
                             component_usage_name = lv_usage
                             create_component     = abap_true
                             do_create            = abap_true
                           ).
      lo_f ?= ls_wd_usage-usage->if_wd_component_usage~get_interface_controller( ).
      lo_f->set_view( io_view_api ).
      DATA(lt_vm) =  wdr_task=>application->get_component_for_id( ls_wd_usage-usage->child_component->id )->view_managers_for_window.
      DATA(lo_vm) = lt_vm[ 1 ]-view_manager.
      lo_vm->window_info->set_is_dynamic( abap_true ).
      DATA(lo_v) = lo_vm->get_view( view_usage = lo_vm->window_info->default_root_vusage ).
      CAST cl_wdr_view( io_view_api )->add_action(
        EXPORTING
          action  = NEW cl_wdr_action(
          controller       = lo_v
          text_key         = 'SEARCH_EXECUTE'
          event_handler    = 'ONACTIONSEARCH_EXECUTE'
        )
          command = 'ZWD_C_TABLE_FIND_SEARCH_EXECUTE'
      ).
      CAST cl_wdr_view( io_view_api )->add_action(
        EXPORTING
          action  = NEW cl_wdr_action(
          controller       = lo_v
          text_key         = 'SEARCH_LEFT'
          event_handler    = 'ONACTIONSEARCH_LEFT'
        )
          command = 'ZWD_C_TABLE_FIND_SEARCH_LEFT'
      ).
      CAST cl_wdr_view( io_view_api )->add_action(
        EXPORTING
          action  = NEW cl_wdr_action(
          controller       = lo_v
          text_key         = 'SEARCH_RIGHT'
          event_handler    = 'ONACTIONSEARCH_RIGHT'
        )
          command = 'ZWD_C_TABLE_FIND_SEARCH_RIGHT'
      ).

      INSERT ls_wd_usage INTO TABLE gt_wd_usage.
    ENDIF.

    ro_wd_usage = ls_wd_usage-usage.

  ENDMETHOD.


  METHOD get_text_markers.
    DATA: ls_text_marker TYPE wdui_c_table_text_marker.
    FIELD-SYMBOLS: <ty_s_hits> TYPE cl_salv_sti_toolbox=>ys_hit.
    CLEAR: et_text_markers.
    LOOP AT mt_toolbox_hits ASSIGNING <ty_s_hits>.
      CLEAR: ls_text_marker.
      ls_text_marker-row_index = <ty_s_hits>-row.
      ls_text_marker-cell_editor_id = mt_cell_editor_id[ <ty_s_hits>-column_idx ].

      "Set focused hit
      IF ir_current_hit IS NOT INITIAL AND <ty_s_hits> = ir_current_hit->*.
        ls_text_marker-is_focused = abap_true.
      ENDIF.
      INSERT ls_text_marker INTO TABLE et_text_markers.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_search_string.
    rv_search_string = mv_search_string.
  ENDMETHOD.


  METHOD get_search_state.
    rv_state = mv_search_state.
  ENDMETHOD.


  METHOD get_ref_of_current_hit.
    FIELD-SYMBOLS: <ty_s_hits> TYPE cl_salv_sti_toolbox=>ys_hit.
    CHECK mv_current_hit_idx > 0.
    READ TABLE mt_toolbox_hits INDEX mv_current_hit_idx ASSIGNING <ty_s_hits>.
    IF sy-subrc = 0.
      GET REFERENCE OF <ty_s_hits> INTO rr_current_hit.
    ENDIF.
  ENDMETHOD.


  METHOD get_new_search_string.

    mo_context_root->get_child_node( if_fpm_guibb_constants=>gc_guibb_list_nodes-dynamic
*    )->get_child_node( if_fpm_guibb_constants=>gc_guibb_list_nodes-table_properties
    )->get_child_node( mo_c_table->id
    )->get_attribute(
      EXPORTING
        name  = if_fpm_guibb_constants=>gc_guibb_tree_attributes-search_string
      IMPORTING
        value = rv_search_string
    ).

  ENDMETHOD.


  METHOD get_last_visi_row.
    DATA: lv_first_visi_row TYPE i,
          visible_row_count TYPE i,
          lv_index          TYPE i.
    lv_first_visi_row = mo_c_table->get_first_visible_row( ).
    visible_row_count = mo_c_table->get_visible_row_count( ).
    lv_index = lv_first_visi_row + visible_row_count - 1.
    rv_row_index = lv_index.
  ENDMETHOD.


  METHOD get_instance.
    DATA: ls_instance TYPE ty_s_instance.

    READ TABLE gt_instance INTO ls_instance WITH KEY c_table = io_c_table.
    IF sy-subrc <> 0.
      ls_instance-c_table = io_c_table.
      CREATE OBJECT ls_instance-instance
        EXPORTING
          io_c_table = io_c_table.
      INSERT ls_instance INTO TABLE gt_instance.
    ENDIF.

    ro_instance = ls_instance-instance.

  ENDMETHOD.


  METHOD get_first_visi_row.
    DATA: lv_first_visi_row TYPE i.
    lv_first_visi_row = mo_c_table->get_first_visible_row( ).
    rv_row_index = lv_first_visi_row.
  ENDMETHOD.


  METHOD execute_search.
    DATA: lr_current_hit TYPE REF TO cl_salv_sti_toolbox=>ys_hit,
          lr_prev_hit    TYPE REF TO cl_salv_sti_toolbox=>ys_hit,
          lv_temp_key    TYPE i,
          lv_start_over  TYPE abap_bool.
    " ----------------------------------------------------------------
    " search state:
    " - before execute_search: not_active, active, to_be_executed,
    "                          jump_to_next or jump_to_prev, hierarchy_changed
    " - after execute_search: active or not_active
    " ----------------------------------------------------------------

    mv_search_string = get_new_search_string( ).
*    if mv_search_string is initial.
*      mv_search_state = gc_search_state-not_active.
*    endif.
    IF mv_search_state <> gc_search_state-not_active.
      IF mv_search_string NE mv_search_string_prev.
        mv_search_state = gc_search_state-to_be_executed.
      ELSE.
        IF mv_search_string IS INITIAL.
          mv_search_state = gc_search_state-not_active.
        ENDIF.
      ENDIF.
    ENDIF.
    CASE mv_search_state.
        " ------------------------------------------------------------
      WHEN gc_search_state-to_be_executed.
        " ------------------------------------------------------------
        mv_search_state = gc_search_state-active.
        mv_current_hit_idx = 0.
        mv_user_changed_data = abap_false.
        " call the ats toolbox api
        trigger_search( ).
        determine_first_hit( ).
        mv_update_frontend_necessary = abap_false.
        lr_current_hit = get_ref_of_current_hit( ).
        IF lr_current_hit IS BOUND.
          update_frontend( ir_current_hit      = lr_current_hit
                           iv_set_text_markers = abap_true ).
          update_first_visi_row( EXPORTING ir_current_hit = lr_current_hit
                                           iv_scroll_type = zcl_wd_c_table_find=>gc_scroll_type-jump_to_initial_hit ).
        ELSE.
          clear_front_end( ).
        ENDIF.
        " ------------------------------------------------------------
      WHEN gc_search_state-jump_to_next.
        " ------------------------------------------------------------
        mv_search_state = gc_search_state-active.
        IF mv_user_changed_data = abap_true.
          " first get previous hit
          lr_prev_hit = get_ref_of_current_hit( ).
          IF lr_prev_hit IS BOUND.
            lv_temp_key = lr_prev_hit->row.
            " refresh the search
            trigger_search( ).
            lv_start_over = determine_next_hit( iv_relative_to_row = lv_temp_key ).
          ENDIF.
        ELSE.
          IF mv_current_hit_idx = 0.
            determine_first_hit( ).
          ELSE.
            lv_start_over = determine_next_hit( ).
          ENDIF.
        ENDIF.
        lr_current_hit = get_ref_of_current_hit( ).
        IF lr_current_hit IS BOUND.
          IF mv_user_changed_data = abap_true.
            update_frontend( ir_current_hit      = lr_current_hit
                             iv_set_text_markers = abap_true ).
          ELSE.
            update_frontend( ir_current_hit      = lr_current_hit
                             iv_set_text_markers = abap_false ).
          ENDIF.
          IF lv_start_over = abap_false.
            update_first_visi_row( EXPORTING ir_current_hit = lr_current_hit
                                             iv_scroll_type = zcl_wd_c_table_find=>gc_scroll_type-jump_to_next ).
          ELSE.
            update_first_visi_row( EXPORTING ir_current_hit = lr_current_hit
                                             iv_scroll_type = zcl_wd_c_table_find=>gc_scroll_type-jump_to_prev ).
          ENDIF.
        ENDIF.
        mv_user_changed_data = abap_false.
        " ------------------------------------------------------------
      WHEN gc_search_state-jump_to_prev.
        " ------------------------------------------------------------
        mv_search_state = gc_search_state-active.
        IF mv_user_changed_data = abap_true.
          " first get previous hit
          lr_prev_hit = get_ref_of_current_hit( ).
          IF lr_prev_hit IS BOUND.
            lv_temp_key = lr_prev_hit->row.
            " refresh the search
            trigger_search( ).
            lv_start_over = determine_prev_hit( iv_relative_to_row = lv_temp_key ).
          ENDIF.
        ELSE.
          IF mv_current_hit_idx = 0.
            determine_first_hit( ).
          ELSE.
            lv_start_over = determine_prev_hit( ).
          ENDIF.
        ENDIF.
        lr_current_hit = get_ref_of_current_hit( ).
        IF lr_current_hit IS BOUND.
          IF mv_user_changed_data = abap_true.
            update_frontend( ir_current_hit      = lr_current_hit
                             iv_set_text_markers = abap_true ).
          ELSE.
            update_frontend( ir_current_hit      = lr_current_hit
                             iv_set_text_markers = abap_false ).
          ENDIF.
          IF lv_start_over = abap_false.
            update_first_visi_row( EXPORTING ir_current_hit = lr_current_hit
                                             iv_scroll_type = zcl_wd_c_table_find=>gc_scroll_type-jump_to_prev ).
          ELSE.
            update_first_visi_row( EXPORTING ir_current_hit = lr_current_hit
                                             iv_scroll_type = zcl_wd_c_table_find=>gc_scroll_type-jump_to_next ).
          ENDIF.
        ENDIF.
        mv_user_changed_data = abap_false.
        " ------------------------------------------------------------
      WHEN gc_search_state-not_active.
        " ------------------------------------------------------------
        " ------------------------------------------------------------
      WHEN gc_search_state-active.
        " ------------------------------------------------------------
        IF mv_user_changed_data = abap_true.
          " refresh the search
          trigger_search( ).
          mv_current_hit_idx = 0.
          IF lines( mt_toolbox_hits ) > 0.
            update_frontend( iv_set_text_markers = abap_true ).
          ELSE.
            clear_front_end( ).
          ENDIF.
        ENDIF.
    ENDCASE.
    update_matches_string( ).
    mv_search_string_prev = mv_search_string.
    mv_user_changed_data = abap_false.
  ENDMETHOD.


  METHOD determine_prev_hit.
    DATA: lv_index TYPE i.
    CHECK mv_current_hit_idx <> 0.
    IF lines( mt_toolbox_hits ) = 1.
      mv_current_hit_idx = 1.
      rv_start_over = abap_false.
    ELSE.
      IF iv_relative_to_row IS NOT INITIAL.
        READ TABLE mt_toolbox_hits WITH KEY row = iv_relative_to_row TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_index = sy-tabix.
          IF lv_index > 1.
            mv_current_hit_idx = lv_index - 1.
            rv_start_over = abap_false.
          ELSE.
            mv_current_hit_idx = lines( mt_toolbox_hits ).
            rv_start_over = abap_true.
          ENDIF.
        ELSE.
          mv_current_hit_idx = 1.
          rv_start_over = abap_false.
        ENDIF.
      ELSE.
        IF mv_current_hit_idx = 1.
          mv_current_hit_idx = lines( mt_toolbox_hits ).
          rv_start_over = abap_true.
        ELSE.
          mv_current_hit_idx = mv_current_hit_idx - 1.
          rv_start_over = abap_false.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD determine_next_hit.
    DATA: lv_index TYPE i.
    CHECK mv_current_hit_idx <> 0.
    IF lines( mt_toolbox_hits ) = 1.
      mv_current_hit_idx = 1.
      rv_start_over = abap_false.
    ELSE.
      IF iv_relative_to_row IS NOT INITIAL.
        READ TABLE mt_toolbox_hits WITH KEY row = iv_relative_to_row TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          lv_index = sy-tabix.
          IF ( lv_index + 1 ) <= lines( mt_toolbox_hits ).
            mv_current_hit_idx = lv_index + 1.
            rv_start_over = abap_false.
          ELSE.
            mv_current_hit_idx = 1.
            rv_start_over = abap_true.
          ENDIF.
        ELSE.
          mv_current_hit_idx = 1.
          rv_start_over = abap_false.
        ENDIF.
      ELSE.
        IF mv_current_hit_idx < lines( mt_toolbox_hits ).
          ADD 1 TO mv_current_hit_idx.
          rv_start_over = abap_false.
        ELSE.
          mv_current_hit_idx = 1.
          rv_start_over = abap_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD determine_first_hit.
    DATA: lv_first_visi_row TYPE i.
    FIELD-SYMBOLS: <ty_s_hits>    TYPE cl_salv_sti_toolbox=>ys_hit.
    " first take care of the simple cases
    IF lines( mt_toolbox_hits ) = 0.
      mv_current_hit_idx = 0.
      RETURN.
    ENDIF.
    IF lines( mt_toolbox_hits ) = 1.
      mv_current_hit_idx = 1.
      RETURN.
    ENDIF.

    " now go on with 2 or more ...
    lv_first_visi_row = get_first_visi_row( ).
    mv_current_hit_idx = 1.
    DATA: lv_hit_found TYPE abap_bool.
    LOOP AT mt_toolbox_hits ASSIGNING <ty_s_hits>.
      IF <ty_s_hits>-row < lv_first_visi_row.
        ADD 1 TO mv_current_hit_idx.
        CONTINUE.
      ELSE.
        lv_hit_found = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.
    IF lv_hit_found = abap_false.
      " that means all hits are in the none visible area of the tree uibb
      " above the current scroll position
      mv_current_hit_idx = 1.
    ENDIF.
  ENDMETHOD.


  METHOD deactivate.
    mv_search_state = gc_search_state-not_active.
    mv_current_hit_idx = 0.
    mv_user_changed_data = abap_false.
    " delete text markers
    DATA: lt_text_markers TYPE wdui_c_table_text_markers.
    IF mo_c_table IS BOUND.
      mo_c_table->set_text_markers( lt_text_markers ).
    ENDIF.
    CLEAR: mv_search_string, mt_toolbox_hits,
           mv_update_frontend_necessary, mt_toolbox_hits.
  ENDMETHOD.


  METHOD create_mt_column_mapping.
    DATA: lt_column         TYPE cl_wd_abstr_c_table_column=>tt_abstr_c_table_column,
          lo_column         TYPE REF TO cl_wd_abstr_c_table_column,
          lo_cell_editor    TYPE REF TO cl_wd_uielement,
          lv_value_path     TYPE string,
          lv_node_path      TYPE string,
          ls_column_mapping TYPE ty_s_column_mapping.

    lv_node_path = mo_c_table->get_data_source( )->get_path( withoutcontroller = abap_true ) && '.'.
    IF lv_node_path CP '1.*'.
      lv_node_path = lv_node_path+2.
    ENDIF.

    lt_column = mo_c_table->get_columns( ).
    LOOP AT lt_column INTO lo_column.
      IF lo_column IS INSTANCE OF cl_wd_c_table_column_group.
        APPEND LINES OF CAST cl_wd_c_table_column_group( lo_column )->get_columns( ) TO lt_column.
        CONTINUE.
      ENDIF.
      CHECK: lo_column IS INSTANCE OF cl_wd_c_table_column.

      CLEAR: ls_column_mapping, lv_value_path.
      ls_column_mapping-ui_column ?= lo_column.
      lo_cell_editor ?= CAST cl_wd_c_table_column( lo_column )->get_table_cell_editor( ).
      CHECK: lo_cell_editor IS NOT INITIAL.
      ls_column_mapping-ui_cell_editor_id = lo_cell_editor->id.

      CASE lo_cell_editor->_cid.
        WHEN cl_wd_button=>cid_button.
          lv_value_path = CAST cl_wd_button( lo_cell_editor )->bound_text( ).
        WHEN cl_wd_caption=>cid_caption.
          lv_value_path = CAST cl_wd_caption( lo_cell_editor )->bound_text( ).
        WHEN cl_wd_checkbox=>cid_checkbox.
          lv_value_path = CAST cl_wd_checkbox( lo_cell_editor )->bound_checked( ).
*        WHEN cl_wd_dropdown_by_idx=>cid_dropdown_by_idx.
*          lv_value_path = CAST cl_wd_dropdown_by_idx( lo_cell_editor )->bound_selected_key( ).
        WHEN cl_wd_dropdown_by_key=>cid_dropdown_by_key.
          lv_value_path = CAST cl_wd_dropdown_by_key( lo_cell_editor )->bound_selected_key( ).
        WHEN cl_wd_drop_down_list_box=>cid_drop_down_list_box.
          lv_value_path = CAST cl_wd_drop_down_list_box( lo_cell_editor )->bound_selected_key( ).
*        WHEN cl_wd_drop_target_cell_editor=>cid_drop_target_cell_editor.
*          lv_value_path = CAST cl_wd_drop_target_cell_editor( lo_cell_editor )->bound_text( ).
*        WHEN cl_wd_file_upload=>cid_file_upload.
*          lv_value_path = CAST cl_wd_file_upload( lo_cell_editor )->bound_text( ).
        WHEN cl_wd_formatted_text_view=>cid_formatted_text_view.
          lv_value_path = CAST cl_wd_formatted_text_view( lo_cell_editor )->bound_text( ).
        WHEN cl_wd_graphical_value_cmp=>cid_graphical_value_cmp.
          lv_value_path = CAST cl_wd_graphical_value_cmp( lo_cell_editor )->bound_bar_value( ).
        WHEN cl_wd_image=>cid_image.
          lv_value_path = CAST cl_wd_image( lo_cell_editor )->bound_source( ).
        WHEN cl_wd_input_field=>cid_input_field.
          lv_value_path = CAST cl_wd_input_field( lo_cell_editor )->bound_value( ).
*        WHEN cl_wd_link=>cid_.
        WHEN cl_wd_file_download=>cid_file_download
          OR cl_wd_link_to_action=>cid_link_to_action
          OR cl_wd_link_to_url=>cid_link_to_url.
          lv_value_path = CAST cl_wd_link( lo_cell_editor )->bound_text( ).
        WHEN cl_wd_progress_indicator=>cid_progress_indicator.
          lv_value_path = CAST cl_wd_progress_indicator( lo_cell_editor )->bound_display_value( ).
        WHEN cl_wd_radiobutton=>cid_radiobutton.
          lv_value_path = CAST cl_wd_radiobutton( lo_cell_editor )->bound_selected_key( ).
        WHEN cl_wd_ratingindicator=>cid_ratingindicator.
          lv_value_path = CAST cl_wd_ratingindicator( lo_cell_editor )->bound_value( ).
        WHEN cl_wd_text_edit=>cid_text_edit.
          lv_value_path = CAST cl_wd_text_edit( lo_cell_editor )->bound_value( ).
        WHEN cl_wd_text_view=>cid_text_view.
          lv_value_path = CAST cl_wd_text_view( lo_cell_editor )->bound_text( ).
        WHEN cl_wd_threshold_slider=>cid_threshold_slider.
          lv_value_path = CAST cl_wd_threshold_slider( lo_cell_editor )->bound_value( ).
        WHEN cl_wd_toggle_button=>cid_toggle_button.
          lv_value_path = CAST cl_wd_toggle_button( lo_cell_editor )->bound_checked( ).
        WHEN cl_wd_tri_state_checkbox=>cid_tri_state_checkbox.
          lv_value_path = CAST cl_wd_tri_state_checkbox( lo_cell_editor )->bound_checked( ).
      ENDCASE.

      CHECK: lv_value_path IS NOT INITIAL.
      REPLACE lv_node_path IN lv_value_path WITH ''.
      CHECK: lv_value_path NS '.'.
      ls_column_mapping-table_field_name = lv_value_path.

      APPEND ls_column_mapping TO mt_column_mapping.
    ENDLOOP.


  ENDMETHOD.


  METHOD create_mo_rtti.
    DATA: lt_context_attr TYPE wdr_context_attr_info_map,
          ls_context_attr TYPE wdr_context_attribute_info,
          lt_comp         TYPE abap_component_tab,
          ls_comp         TYPE abap_componentdescr.

    mo_rtti = mo_c_table->get_data_source( )->get_node_info( )->get_static_attributes_type( ).
    IF mo_rtti IS NOT INITIAL.
      mv_is_rtti_static = abap_true.
    ELSE.
      lt_context_attr = mo_c_table->get_data_source( )->get_node_info( )->get_attributes( ).
      CHECK: lt_context_attr IS NOT INITIAL.
      LOOP AT lt_context_attr INTO ls_context_attr.
        ls_comp-name = ls_context_attr-name.
        ls_comp-type = ls_context_attr-rtti.
        APPEND ls_comp TO lt_comp.
      ENDLOOP.
      mo_rtti ?= cl_abap_structdescr=>create( lt_comp ).
    ENDIF.

  ENDMETHOD.


  METHOD create_dynamic_node.
    " -------------------------------------------------------------------
    " create node DYNAMIC.TABLE_PROPERTIES
    " -------------------------------------------------------------------
    DATA: lo_config_root TYPE REF TO if_wd_context_node.
    lo_config_root = mo_context_root.

    DATA lo_context_node_info TYPE REF TO if_wd_context_node_info.
    lo_context_node_info = lo_config_root->get_node_info( ).

    DATA lo_dynamic_node_info TYPE REF TO if_wd_context_node_info.
    TRY.
        lo_dynamic_node_info = lo_context_node_info->get_child_node( if_fpm_guibb_constants=>gc_guibb_list_nodes-dynamic ).
      CATCH cx_wd_context.
        lo_dynamic_node_info = lo_context_node_info->add_new_child_node(
          name         = if_fpm_guibb_constants=>gc_guibb_list_nodes-dynamic
          is_static    = abap_false
          is_multiple  = abap_false
          is_singleton = abap_true
          is_mandatory = abap_true ).
    ENDTRY.


    DATA lo_table_props_node_info TYPE REF TO if_wd_context_node_info.
    lo_table_props_node_info = lo_dynamic_node_info->add_new_child_node(
*      name         = if_fpm_guibb_constants=>gc_guibb_list_nodes-table_properties
      name         = mo_c_table->id
      is_static    = abap_false
      is_multiple  = abap_false
      is_singleton = abap_true
      is_mandatory = abap_true ).

    DATA ls_attribute_info TYPE wdr_context_attribute_info.

    " -------------------------------------------------------------------
    " Attribute for search string
    " -------------------------------------------------------------------
    ls_attribute_info-name = if_fpm_guibb_constants=>gc_guibb_tree_attributes-search_string.
    ls_attribute_info-type_name = 'STRING'.                 "#EC NOTEXT
    ls_attribute_info-rtti ?=
      cl_abap_datadescr=>describe_by_name( 'STRING' ).
    lo_table_props_node_info->add_attribute( ls_attribute_info ).

    " -------------------------------------------------------------------
    " Attributes for search matches
    " -------------------------------------------------------------------
    " text
    ls_attribute_info-name = if_fpm_list_types=>cs_list_attribute_internal-search_text_matches.
    ls_attribute_info-type_name = 'STRING'.                 "#EC NOTEXT
    ls_attribute_info-rtti ?= cl_abap_datadescr=>describe_by_name( 'STRING' ).
    lo_table_props_node_info->add_attribute( ls_attribute_info ).
    " visibility
    ls_attribute_info-name = if_fpm_list_types=>cs_list_attribute_internal-search_text_matches_visible.
    ls_attribute_info-type_name = 'WDUI_VISIBILITY'.        "#EC NOTEXT
    ls_attribute_info-rtti ?= cl_abap_datadescr=>describe_by_name( 'WDUI_VISIBILITY' ).
    lo_table_props_node_info->add_attribute( ls_attribute_info ).

  ENDMETHOD.


  METHOD create.
    get_wd_usage( io_view_api ).

    IF io_c_table IS NOT INITIAL.
      get_instance( io_c_table ).
    ELSE.
      DATA(lt_uiel) = CAST cl_wdr_view( io_view_api )->get_elements_by_cid( cl_wd_c_table=>cid_c_table ).
      LOOP AT lt_uiel INTO DATA(lo_uiel).
        get_instance( CAST cl_wd_c_table( lo_uiel ) ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    DATA: lo_toolbox        TYPE REF TO if_salv_itab_toolbox,
          lo_column_catalog TYPE REF TO if_salv_column_catalog,
          lo_usage          TYPE REF TO cl_wdr_component_usage.

    mv_search_state = gc_search_state-not_active.
    mo_c_table = io_c_table.
    mo_view = mo_c_table->view.
    mo_context_root = mo_view->if_wd_controller~get_context( )->root_node.

    lo_usage = get_wd_usage( mo_view ).
    DATA(lo_p) = wdr_task=>application->get_component_for_id( lo_usage->parent_component->id ).
    IF lo_p->parent IS NOT INITIAL.
      mo_parent_usage = lo_p->parent->component->get_component_usage( lo_p->component_usage_name ).
      SET HANDLER on_delete FOR mo_parent_usage.
    ENDIF.

    create_mo_rtti( ).
    IF mo_rtti IS INITIAL.
      DELETE gt_instance WHERE c_table = mo_c_table.
      RETURN.
    ENDIF.
    create_mt_column_mapping( ).

    cl_salv_itab_services=>create_toolbox_with_catalog(
      EXPORTING io_structdescr    = mo_rtti
      IMPORTING eo_toolbox        = lo_toolbox
                eo_column_catalog = lo_column_catalog ).
    mo_toolbox ?= lo_toolbox.
    configure_columns( lo_column_catalog ).

    create_dynamic_node( ).
    render_search_area( ).
  ENDMETHOD.


  METHOD configure_columns.
    DATA: lt_salv_code_list    TYPE if_salv_service_types=>yt_codelist,
          ls_salv_code_list    LIKE LINE OF lt_salv_code_list,
          lv_salv_col_name     TYPE if_salv_service_types=>y_column_name,
          lv_salv_col_ref_name TYPE if_salv_service_types=>y_column_name,
          lo_node_info         TYPE REF TO cl_wdr_context_node_info,
          lt_attribute_info    TYPE wdr_context_attr_info_map,
          ls_attribute_info    TYPE wdr_context_attribute_info,
          ls_column_mapping    TYPE ty_s_column_mapping.
    FIELD-SYMBOLS: <wdr_context_attr_value>  TYPE wdr_context_attr_value.

    CHECK io_column_catalog IS BOUND.
    lo_node_info ?= mo_c_table->get_data_source( )->get_node_info( ).
    lt_attribute_info = lo_node_info->get_attributes_internal( ).


    LOOP AT mt_column_mapping INTO ls_column_mapping.
      READ TABLE lt_attribute_info INTO ls_attribute_info WITH KEY name = ls_column_mapping-table_field_name.
      CHECK: sy-subrc EQ 0.

      " ---------------------------------------------------------------
      " fixed value set
      " ---------------------------------------------------------------
      IF ls_attribute_info-value_set IS NOT INITIAL.
        CLEAR: lt_salv_code_list.
        LOOP AT ls_attribute_info-value_set ASSIGNING <wdr_context_attr_value>.
          ls_salv_code_list-code        = <wdr_context_attr_value>-value.
          ls_salv_code_list-description = <wdr_context_attr_value>-text.
          APPEND ls_salv_code_list TO lt_salv_code_list.
        ENDLOOP.
        lv_salv_col_name = ls_column_mapping-table_field_name.
        TRY.
            io_column_catalog->set_column_codelist(
              EXPORTING
                i_column_name             = lv_salv_col_name
                it_codelist               = lt_salv_code_list
            )->set_formatting( if_salv_coltype_codelist=>cs_presentation_mode-description ).
          CATCH cx_salv_column_definition ##NO_HANDLER.
        ENDTRY.
      ENDIF.

      " ---------------------------------------------------------------
      " reference fields
      " ---------------------------------------------------------------
      IF NOT ls_attribute_info-reference_field_type IS INITIAL AND
         NOT ls_attribute_info-reference_field IS INITIAL.
        TRY.
            CASE ls_attribute_info-reference_field_type.
              WHEN 'C' OR 'c'.
                lv_salv_col_name = ls_column_mapping-table_field_name.
                lv_salv_col_ref_name = ls_attribute_info-reference_field.
                io_column_catalog->set_column_amount( EXPORTING i_column_name         = lv_salv_col_name
                                                                i_currency_field_path = lv_salv_col_ref_name ).
              WHEN 'Q' OR 'q'.
                lv_salv_col_name = ls_column_mapping-table_field_name.
                lv_salv_col_ref_name = ls_attribute_info-reference_field.
                io_column_catalog->set_column_quantity( EXPORTING i_column_name = lv_salv_col_name
                                                        i_unit_field_path       =  lv_salv_col_ref_name ).
            ENDCASE.
          CATCH cx_salv_column_definition ##NO_HANDLER.
        ENDTRY.
      ENDIF.

    ENDLOOP.
  ENDMETHOD.


  METHOD clear_front_end.
    DATA: lt_text_markers TYPE wdui_c_table_text_markers.
    mo_c_table->set_text_markers( lt_text_markers ).
    mv_update_frontend_necessary = abap_false.
  ENDMETHOD.
ENDCLASS.
