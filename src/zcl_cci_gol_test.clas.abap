CLASS zcl_cci_gol_test DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.



    methods testdummy.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_cci_gol_test IMPLEMENTATION.


  METHOD testdummy.

    DATA(lo_gol) = new zcl_cci_gol(  ).


    TRY.
     lo_gol->set_alive( iv_x = 1 iv_y = 2 iv_alive = abap_true ).

    catch cx_static_check.

    endtry.



  ENDMETHOD.

ENDCLASS.
