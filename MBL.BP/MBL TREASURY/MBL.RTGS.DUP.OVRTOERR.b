* @ValidationCode : MjoxMTEzODYwMDE4OkNwMTI1MjoxNjI2MzUwOTM1NTgxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 15 Jul 2021 18:08:55
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE MBL.RTGS.DUP.OVRTOERR
*************************************************
*Company Name :
*Developed By : K M ZAHID MAHMUD
*
*----------------------------------------------------------------------
*Subroutine Type:
*
*Attached To : FUNDS.TRANSFER
*
*Attached As : Input Routine
*
*In Parameter :
*
*Out Parameter :

*Description: This routine will convert override message to error message for probable dublicate FT
*----------------------------------------------------------------------
*Modification Details:
*New:
*
*-------------------------------------------------------------------------
*Modification History :
******************************************************************
    $INSERT I_COMMON
    $INSERT I_EQUATE
    
* $INSERT T24.BP I_F.FUNDS.TRANSFER
    $USING FT.Contract
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    !DEBUG

    GOSUB INITIALISE
    GOSUB MAIN.PROCESS
RETURN

**********
INITIALISE:
**********


    FN.FTR='F.FUNDS.TRANSFER'
    F.FTR=''

RETURN

*************
MAIN.PROCESS:
*************

* Y.OVERRIDE.VAL = R.NEW(V-9)
    Y.OVERRIDE.VAL = EB.SystemTables.getRNew(V-9)
    Y.OVRRD.NO = DCOUNT(Y.OVERRIDE.VAL,@VM)

    Y.OVRRD.ID = ''



*DUP.CONTRACT



    FOR I=1 TO Y.OVRRD.NO
        Y.OVRRD.DETLS = FIELD(Y.OVERRIDE.VAL,@VM,I)
        Y.OVRRD.ID = FIELD(Y.OVRRD.DETLS,'}',1)
        IF (Y.OVRRD.ID EQ 'DUP.CONTRACT') THEN
            GOSUB STOP.DUPLI.FT
            BREAK
        END

        !
    NEXT I

RETURN

STOP.DUPLI.FT:
* AF = I
    EB.SystemTables.getAf()

*    ETEXT = 'Duplicate FT, Contact with RTGS Admin.'
*    CALL STORE.END.ERROR
    
    EB.SystemTables.setEtext('Duplicate FT, Contact with RTGS Admin.')
    EB.ErrorProcessing.StoreEndError()
 
RETURN


END
