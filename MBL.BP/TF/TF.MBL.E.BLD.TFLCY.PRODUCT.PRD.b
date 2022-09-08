SUBROUTINE TF.MBL.E.BLD.TFLCY.PRODUCT.PRD(ENQ.DATA)
*-----------------------------------------------------------------------------
*Subroutine Description: This build routine to alter the selection list of PRODUCT
*Subroutine Type:
*Attached To    : MBL.ENQ.AA.ARR.TFLCY-NAU
*Attached As    : BUILD ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 12/07/2021 -                            Create   - MD. EBRAHIM KHALIL RIAN
*                                                 FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INCLUDE I_ENQUIRY.COMMON
*-----------------------------------------------------------------------------
    GOSUB PROCESS ; *PROCESS BUSINESS LOGIC
*-----------------------------------------------------------------------------
  
*** <region name= PROCESS>
PROCESS:
*** <desc>PROCESS BUSINESS LOGIC </desc>
    Y.FIELDS=ENQ.DATA<2>
    Y.POS=DCOUNT(Y.FIELDS,@VM)+1
    ENQ.DATA<2,Y.POS>= "PRODUCT"
    ENQ.DATA<3,Y.POS>= "EQ"
    ENQ.DATA<4,Y.POS>= "MBL.LOCAGENTCOM.AC MBL.MARGINLC.AC MBL.RESERVEMARGIN.AC MBL.TFSETTLEMENT.AC MBL.NITA.AC MBL.NRTA.AC"
RETURN
*** </region>

END
