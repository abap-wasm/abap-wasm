INTERFACE zif_wasm_sections PUBLIC.

  CONSTANTS:
* Note that these constants are not structured as they contain JS keywords
      gc_section_custom   TYPE x LENGTH 1 VALUE '00' ##NO_TEXT.
  CONSTANTS:
      gc_section_type     TYPE x LENGTH 1 VALUE '01' ##NO_TEXT.
  CONSTANTS:
      gc_section_import   TYPE x LENGTH 1 VALUE '02' ##NO_TEXT.
  CONSTANTS:
      gc_section_function TYPE x LENGTH 1 VALUE '03' ##NO_TEXT.
  CONSTANTS:
      gc_section_table    TYPE x LENGTH 1 VALUE '04' ##NO_TEXT.
  CONSTANTS:
      gc_section_memory   TYPE x LENGTH 1 VALUE '05' ##NO_TEXT.
  CONSTANTS:
      gc_section_global   TYPE x LENGTH 1 VALUE '06' ##NO_TEXT.
  CONSTANTS:
      gc_section_export   TYPE x LENGTH 1 VALUE '07' ##NO_TEXT.
  CONSTANTS:
      gc_section_start    TYPE x LENGTH 1 VALUE '08' ##NO_TEXT.
  CONSTANTS:
      gc_section_element  TYPE x LENGTH 1 VALUE '09' ##NO_TEXT.
  CONSTANTS:
      gc_section_code     TYPE x LENGTH 1 VALUE '0A' ##NO_TEXT.
  CONSTANTS:
      gc_section_data     TYPE x LENGTH 1 VALUE '0B' ##NO_TEXT.
  CONSTANTS:
      gc_section_data_count TYPE x LENGTH 1 VALUE '0C' ##NO_TEXT.

ENDINTERFACE.
