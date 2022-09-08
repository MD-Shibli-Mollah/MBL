SUBROUTINE GB.MBL.I.DUP.CUS.CHK
*-----------------------------------------------------------------------------
*
* Company Name   : FDS Bangladesh Pvt Ltd
* Developed By   : Md.Kamrul Hasan - Software Engineer
* Date & Time    : 10 jul 2021 ,11.00 Am
*----------------------------------------------------------------------
*Subroutine Type: Validation Routine
*
*Attached To    : Customer Version (CUSTOMER,MBL.INDV)
*
*Attached As    : INPUT ROUTINE
*
*In Parameter   :
*
*Out Parameter  :

*------------------------------------------
*Description
*------------
* This Routine validates Duplicate customer Available in system and makes it Error By getting the Override Value
*------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_SOA.COMMON
    $USING EB.SystemTables
    $USING EB.OverrideProcessing
    $USING EB.DataAccess
    $USING EB.ErrorProcessing
    $USING ST.Customer
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *
    GOSUB OPENFILE ; *
    GOSUB PROCESS ; *
   
RETURN
*-----------------------------------------------------------------------------
*** <region name= INITIALISE>
INITIALISE:
*    FN.OVERRIDE = 'F.OVERRIDE'
*    F.OVERRIDE  = ''
*** <desc> </desc>
   
    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
   
RETURN
*** </region>
*** <region name= OPENFILE>
OPENFILE:
*** <desc> </desc>
*EB.DataAccess.Opf(FN.OVERRIDE, F.OVERRIDE)
    EB.DataAccess.Opf(FN.CUS, F.CUS)

RETURN
*** </region>
*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>
       
    Y.CUS.ID = EB.SystemTables.getIdNew()

    Y.OVERRIDE.ID = EB.OverrideProcessing.getOveOverrideId()
    IF Y.OVERRIDE.ID EQ '' THEN
        Y.OVERRIDE.ID = EB.SystemTables.getRNew(EB.SystemTables.getV()-9)
    END
    
    Y.ACC.NO = FIELD(Y.OVERRIDE.ID,'{',2)
    Y.OVERRIDE.ID = FIELD(Y.OVERRIDE.ID,'}',1)
    
    EB.DataAccess.FRead(FN.CUS, Y.CUS.ID, CUS.REC, F.CUS, CUS.ERR)
    Y.CUR.NO = CUS.REC<ST.Customer.Customer.EbCusCurrNo>
    
    IF Y.OVERRIDE.ID EQ 'DUP.CONTRACT' THEN
        IF NOT(Y.CUR.NO) THEN
            EB.SystemTables.setEtext('POSSIBLE DUPLICATE CONTRACT ':Y.ACC.NO)
            EB.ErrorProcessing.StoreEndError()
        END
    END
    
RETURN
*** </region>

END