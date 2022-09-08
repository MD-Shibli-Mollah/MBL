* @ValidationCode : MjoxNTcxNjczNzY5OkNwMTI1MjoxNjI1NzIzNzI4Nzg0OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Jul 2021 11:55:28
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.CR.LOCKING.WRITE
*-----------------------------------------------------------------------------
*Subroutine Description: USED TO UPDATE LOCKING FILE WHILE CREATING TELLER VERSION RECORD ID
*Subroutine Type:
*Attached To    : TELLER VERSION
*Attached As    : CHECK RECORD  ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 14/06/2020 -                            Retrofit   - MD.KAMRUL HASAN
*                                                 FDS Pvt Ltd
*-----------------------------------------------------------------------------
* Modification History : Unnecessary INSERTS are commented out.
* 1)
*  Date : 08 JULY 2021
*  Modification Description : LINE NO: 34-38 are COMMENTED.
*     Y.TODAY,Y.APP,Y.VERSION,Y.SYS.LOCK,Y.LEN.NUM variables are assigned for System imports.
*  Modified By  : MD SHIBLI MOLLAH - FDS BD
*
*-----------------------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*    $INSERT I_ENQUIRY.COMMON
*    $INSERT I_GTS.COMMON
*    $USING ST.CompanyCreation
*    $USING ST.Config
*    $USING EB.Updates
    $USING FT.Contract
    $USING TT.Contract
    $USING EB.DataAccess
    $USING EB.SystemTables

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

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
    
    Y.APP = EB.SystemTables.getApplication()
    Y.VERSION = EB.SystemTables.getPgmVersion()
    Y.APP.VER.NAME = Y.APP : Y.VERSION
    
    Y.SYS.LOCK = EB.SystemTables.getFLocking()
    
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

    IF Y.APP EQ "FUNDS.TRANSFER" THEN
        EB.DataAccess.FRead(FN.FT.NAU,Y.ID,FT.NAU.REC,F.FT.NAU,FT.NAU.ERR)
    END

    IF Y.APP EQ "TELLER" THEN
        EB.DataAccess.FRead(FN.TT.NAU,Y.ID,TT.NAU.REC,F.TT.NAU,TT.NAU.ERR)
    END

    IF FT.NAU.REC OR TT.NAU.REC THEN

    END ELSE

        Y.LOCKING.ID = 'SS' : Y.TODAY
        READU R.LOCK.REC.CNT FROM Y.SYS.LOCK,Y.LOCKING.ID THEN
            R.LOCK.REC.CNT<1> += 1
        END ELSE
            R.LOCK.REC.CNT<1> = 1
        END

        Y.SERIAL.NO1 = R.LOCK.REC.CNT<1> + 0
        Y.SERIAL.NO1 = FMT(Y.SERIAL.NO1,'R%5')
        Y.LEN.NUM = LEN(Y.SERIAL.NO1)
        WRITE R.LOCK.REC.CNT TO Y.SYS.LOCK,Y.LOCKING.ID

        BEGIN CASE
            CASE Y.LEN.NUM EQ 1
                Y.SERIAL.NO = "0000":Y.SERIAL.NO1
            CASE Y.LEN.NUM EQ 2
                Y.SERIAL.NO = "000":Y.SERIAL.NO1
            CASE Y.LEN.NUM EQ 3
                Y.SERIAL.NO = "00":Y.SERIAL.NO1
            CASE Y.LEN.NUM EQ 4
                Y.SERIAL.NO = "0":Y.SERIAL.NO1
            CASE Y.LEN.NUM EQ 5
                Y.SERIAL.NO = Y.SERIAL.NO1
        END CASE

        Y.REF.ID = 'SS' : Y.TODAY : Y.SERIAL.NO

        IF Y.VERSION EQ ",MBL.SUSP.ORG" THEN
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitTheirRef, Y.REF.ID)
        END

        IF Y.VERSION EQ ",MBL.SUNDRY.ORG" THEN
            EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditTheirRef, Y.REF.ID)
        END

        IF Y.VERSION EQ ",MBL.SDSA.LCY.CASHIN" OR Y.VERSION EQ ",MBL.SDSA.LCY.CASHWDL.SUSP" THEN
            EB.SystemTables.setRNew(TT.Contract.Teller.TeOurReference, Y.REF.ID)
        END
    END

RETURN
END
