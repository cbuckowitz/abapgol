CLASS zcl_cci_gol DEFINITION
  PUBLIC
  CREATE PUBLIC.


  PUBLIC SECTION.

    TYPES t_int TYPE int4.
    TYPES tt_int TYPE STANDARD TABLE OF t_int WITH DEFAULT KEY.
    TYPES t_bool TYPE boolean.


    TYPES: BEGIN OF ts_field_cell,
             x      TYPE t_int,
             y      TYPE t_int,
             alive  TYPE t_bool,
             future TYPE t_bool,
           END OF ts_field_cell.


    METHODS: init_field IMPORTING width TYPE t_int height TYPE t_int.

    METHODS: get_alive
      IMPORTING
                iv_x            TYPE t_int
                iv_y            TYPE t_int
      RETURNING VALUE(rv_alive) TYPE t_bool
      RAISING   cx_static_check.

    METHODS: set_alive
      IMPORTING
                iv_x     TYPE t_int
                iv_y     TYPE t_int
                iv_alive TYPE t_bool
      RAISING   cx_static_check.

    METHODS: get_cell_count RETURNING VALUE(rv_count) TYPE t_int.

    METHODS: count_neighbors
      IMPORTING
                iv_x            TYPE t_int
                iv_y            TYPE t_int
      RETURNING VALUE(rv_count) TYPE t_int.

    METHODS: compute_future
      IMPORTING
                iv_alive         TYPE t_bool
                iv_neighbors     TYPE t_int
      RETURNING VALUE(rv_future) TYPE t_bool
      RAISING
                cx_static_check.

    METHODS: play_a_round
      RAISING
        cx_static_check.


  PROTECTED SECTION.

    DATA mt_field TYPE HASHED TABLE OF ts_field_cell WITH UNIQUE KEY x y.


  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_cci_gol IMPLEMENTATION.



  METHOD init_field.

    DATA lv_x TYPE int4.
    DATA lv_y TYPE int4.


    CLEAR mt_field.

    DO height TIMES.
      ADD 1 TO lv_y.
      CLEAR lv_x.

      DO width TIMES.
        ADD 1 TO lv_x.

        INSERT VALUE #( x = lv_x  y = lv_y alive = abap_false ) INTO TABLE mt_field.

      ENDDO.

    ENDDO.

  ENDMETHOD.




  METHOD set_alive.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_field.


    READ TABLE mt_field ASSIGNING <ls_field> WITH KEY x = iv_x y = iv_y.
    IF sy-subrc > 0.
      RAISE EXCEPTION TYPE lcx_field_not_found.
    ENDIF.

    <ls_field>-alive = iv_alive.

  ENDMETHOD.


  METHOD get_alive.
    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_field.


    READ TABLE mt_field ASSIGNING <ls_field> WITH KEY x = iv_x y = iv_y.
    IF sy-subrc > 0.
      RAISE EXCEPTION TYPE lcx_field_not_found.
    ENDIF.

    rv_alive = <ls_field>-alive.
  ENDMETHOD.

  METHOD get_cell_count.
    rv_count = lines( mt_field ).
  ENDMETHOD.





  METHOD count_neighbors.

    FIELD-SYMBOLS <lv_x> TYPE t_int.
    FIELD-SYMBOLS <lv_y> TYPE t_int.


    LOOP AT VALUE tt_int( ( -1 ) ( 0 ) ( 1 ) ) ASSIGNING <lv_y>.

      LOOP AT VALUE tt_int( ( -1 ) ( 0 ) ( 1 ) ) ASSIGNING <lv_x>.


        CHECK <lv_x> NE 0 OR <lv_y> NE 0.

        TRY.
            IF get_alive( iv_x = iv_x + <lv_x> iv_y = iv_y + <lv_y> ) EQ abap_true.
              ADD 1 TO rv_count.
            ENDIF.

          CATCH lcx_field_not_found.
        ENDTRY.

      ENDLOOP.
    ENDLOOP.


  ENDMETHOD.


  METHOD compute_future.


    IF iv_alive EQ abap_true.
*    Feld mit Zelle

      CASE iv_neighbors.

        WHEN 2 OR 3.
          rv_future = abap_true.

        WHEN OTHERS.
          rv_future = abap_false.

      ENDCASE.

    ELSE.
*    Feld ist leer

      CASE iv_neighbors.

        WHEN 3.
          rv_future = abap_true.

        WHEN OTHERS.
          rv_future = abap_false.

      ENDCASE.

    ENDIF.


  ENDMETHOD.




  METHOD play_a_round.

    FIELD-SYMBOLS <ls_field> LIKE LINE OF mt_field.


*    Zukunft für jede Zelle berechnen
    LOOP AT mt_field ASSIGNING <ls_field>.

      <ls_field>-future = compute_future(
              iv_alive = <ls_field>-alive
              iv_neighbors = count_neighbors(
                  iv_x = <ls_field>-x
                  iv_y = <ls_field>-y
          )
      ).

    ENDLOOP.

*    to the future
    LOOP AT mt_field ASSIGNING <ls_field>.

      <ls_field>-alive = <ls_field>-future.

    ENDLOOP.


  ENDMETHOD.

ENDCLASS.
