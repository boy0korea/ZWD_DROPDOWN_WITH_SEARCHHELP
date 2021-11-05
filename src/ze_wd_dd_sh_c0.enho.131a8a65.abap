"Name: \PR:/1WDA/C0STANDARD==============CP\TY:CL_DROPDOWN_BY_KEY\IN:IF_NW7_VIEW_ELEMENT_ADAPTER\ME:SET_CONTENT\SE:BEGIN\EI
ENHANCEMENT 0 ZE_WD_DD_SH_C0.
*
    IF zcl_wd_dd_sh=>gv_on EQ abap_true.
      IF zmo_sh IS INITIAL.
        create object zmo_sh type cl_input_field
          exporting
            i_type_id = /1wda/c0standard=>mc_aid_input_field
            i_parent  = me.
        zmo_sh->m_view_element = zcl_wd_dd_sh=>enh_set_content( m_view_element ).
      ENDIF.

      zmo_sh->m_context_element = m_context_element.
      zmo_sh->m_context_node_path_name = m_context_node_path_name.
      zmo_sh->if_nw7_view_element_adapter~set_content( ).
    ENDIF.
ENDENHANCEMENT.
