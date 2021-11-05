"Name: \PR:/1WDA/C0STANDARD==============CP\TY:CL_INPUT_FIELD\IN:IF_NW7_VIEW_ELEMENT_ADAPTER\ME:HANDLE_EVENT\SE:BEGIN\EI
ENHANCEMENT 0 ZE_WD_DD_SH_C0.
*
    IF m_parent IS INSTANCE OF cl_dropdown_by_key AND
       CAST cl_dropdown_by_key( m_parent )->zmo_sh EQ me AND
       i_event_name EQ 'FieldHelpPress'.
      zcl_wd_dd_sh=>enh_handle_event(
        EXPORTING
          m_view_element   = m_parent->m_view_element
          m_context_element = m_parent->m_context_element
      ).
    ENDIF.
ENDENHANCEMENT.
