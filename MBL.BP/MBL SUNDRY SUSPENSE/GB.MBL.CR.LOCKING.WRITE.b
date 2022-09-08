SUBROUTINE  GB.MBL.CR.LOCKING.WRITE
*
* Company Name   : FDS Bangladesh Pvt Ltd
* Modified By   : Md.Kamrul Hasan - Software Engineer
*----------------------------------------------------------------------
*Subroutine Type: Validation Routine
*
*Attached To    : TELLER VERSION
*
*Attached As    : CHECK RECORD ROUTINE
*
*In Parameter   :
*
*Out Parameter  :

*------------------------------------------
*Description
*------------
*This routine for update reference no in Sundry/Suspense
*------------------------------------------------------------------------------------
*------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_GTS.COMMON
    $INSERT I_EB.TRANS.COMMON
    $USING ST.CompanyCreation
    $USING FT.Contract
    $USING TT.Contract
    $USING ST.Config
    $USING EB.DataAccess
    $USING EB.Updates
    $USING EB.SystemTables
    $USING EB.Logging
    $USING EB.TransactionControl
    

    IF cTxn_TransactionLevel LT 99 AND OFS$OPERATION EQ 'BUILD' THEN
        GOSUB INIT
        GOSUB OPENFILES
        GOSUB PROCESS
    END

RETURN


INIT:

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''

    FN.FT.NAU = 'F.FUNDS.TRANSFER$NAU'
    F.FT.NAU = ''

    FN.TT = 'F.TELLER'
    F.TT = ''

    FN.TT.NAU = 'F.TELLER$NAU'
    F.TT.NAU = ''

    FN.CAT = 'F.CATEGORY'
    F.CAT = ''


    FN.LOC = 'F.LOCKING'
    F.LOC = ''

    Y.FT.ORGADJ.POS = ''
    Y.FT.AL.POS = ''
    Y.TT.ORGADJ.POS = ''
    Y.TT.AL.POS = ''
    Y.CAT.AL.POS = ''


    Y.TODAY = EB.SystemTables.getToday()
    Y.YEAR = Y.TODAY[3,2]

    Y.APP.VER.NAME = EB.SystemTables.getApplication() : EB.SystemTables.getPgmVersion()
    
RETURN

OPENFILES:
    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.FT.NAU,F.FT.NAU)
    EB.DataAccess.Opf(FN.TT,F.TT)
    EB.DataAccess.Opf(FN.TT.NAU,F.TT.NAU)
    EB.DataAccess.Opf(FN.CAT,F.CAT)

    OPEN 'F.LOCKING' TO F.LOCKING ELSE F.LOCKING = ''
RETURN

PROCESS:
    Y.ID = EB.SystemTables.getIdNew()

    IF EB.SystemTables.getApplication() EQ "FUNDS.TRANSFER" THEN
        EB.DataAccess.FRead(FN.FT.NAU,Y.ID,FT.NAU.REC,F.FT.NAU,FT.NAU.ERR)
    END

    IF EB.SystemTables.getApplication() EQ "TELLER" THEN
        EB.DataAccess.FRead(FN.TT.NAU,Y.ID,TT.NAU.REC,F.TT.NAU,TT.NAU.ERR)
    END

    IF FT.NAU.REC OR TT.NAU.REC THEN

    END ELSE

        Y.LOCKING.ID = 'SS' : EB.SystemTables.getToday()
        READU R.LOCK.REC.CNT FROM EB.SystemTables.getFLocking(),Y.LOCKING.ID THEN
            R.LOCK.REC.CNT<1> += 1
        END ELSE
            R.LOCK.REC.CNT<1> = 1
        END

        Y.SERIAL.NO1 = R.LOCK.REC.CNT<1> + 0
        Y.SERIAL.NO1 = FMT(Y.SERIAL.NO1,'R%5')
*WRITE R.LOCK.REC.CNT TO EB.SystemTables.getFLocking(),Y.LOCKING.ID
        EB.Logging.LogWrite(FN.LOC, Y.LOCKING.ID, R.LOCK.REC.CNT, FlushIt)
        EB.TransactionControl.JournalUpdate('')
        SENSITIVITY =''

        BEGIN CASE
            CASE LEN(Y.SERIAL.NO1) EQ 1
                Y.SERIAL.NO = "0000":Y.SERIAL.NO1
            CASE LEN(Y.SERIAL.NO1) EQ 2
                Y.SERIAL.NO = "000":Y.SERIAL.NO1
            CASE LEN(Y.SERIAL.NO1) EQ 3
                Y.SERIAL.NO = "00":Y.SERIAL.NO1
            CASE LEN(Y.SERIAL.NO1) EQ 4
                Y.SERIAL.NO = "0":Y.SERIAL.NO1
            CASE LEN(Y.SERIAL.NO1) EQ 5
                Y.SERIAL.NO = Y.SERIAL.NO1
        END CASE

        Y.REF.ID = 'SS' : EB.SystemTables.getToday() : Y.SERIAL.NO

        IF EB.SystemTables.getPgmVersion() EQ ",MBL.SUSP.ORG" THEN
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitTheirRef, Y.REF.ID)
        END

        IF EB.SystemTables.getPgmVersion() EQ ",MBL.SUNDRY.ORG" THEN
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditTheirRef, Y.REF.ID)
        END

        IF EB.SystemTables.getPgmVersion() EQ ",MBL.SDSA.LCY.CASHIN" OR EB.SystemTables.getPgmVersion() EQ ",MBL.SDSA.LCY.CASHWDL.SUSP" THEN
            EB.SystemTables.setRNew(TT.Contract.Teller.TeOurReference, Y.REF.ID)
        END
    END
RETURN
END
