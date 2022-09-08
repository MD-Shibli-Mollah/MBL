* @ValidationCode : MjotODMzMjIxODM0OkNwMTI1MjoxNjI1ODU0NDgwOTA5OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Jul 2021 00:14:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.I.SDSA.AC.VER.CHECK

*Subroutine Description: INPUT Routine for Sundry Suspence of TT & FT
*Subroutine Type:
*Attached To    :  VERSION.CONTROL of TELLER & FUNDS.TRANSFER
*Attached As    :  Input Routine
*-----------------------------------------------------------------------------
* Modification History 1 :
* 09/07/2021 -                            Retrofit   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    
    $USING AC.AccountOpening
    $USING TT.Contract
    $USING FT.Contract
* $INSERT T24.BP I_F.CATEGORY
    $USING ST.Config
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING EB.ErrorProcessing
    $INSERT I_F.BD.SDSA.ENTRY.DETAILS

    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS

RETURN

INIT:

    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''

    FN.TT = 'F.TELLER'
    F.TT = ''

    FN.CAT = 'F.CATEGORY'
    F.CAT = ''

    FN.SDSA.ENTRY = 'F.BD.SDSA.ENTRY.DETAILS'
    F.SDSA.ENTRY = ''

    FN.AC = 'F.ACCOUNT'
    F.AC = ''

RETURN

OPENFILES:

    EB.DataAccess.Opf(FN.FT,F.FT)
    EB.DataAccess.Opf(FN.TT,F.TT)
    EB.DataAccess.Opf(FN.CAT,F.CAT)
    EB.DataAccess.Opf(FN.SDSA.ENTRY,F.SDSA.ENTRY)
    EB.DataAccess.Opf(FN.AC,F.AC)

RETURN

PROCESS:

******************************
    !Validaton for TT Application:
******************************
    Y.APP = EB.SystemTables.getApplication()
    Y.VERSION = EB.SystemTables.getPgmVersion()
    
* IF APPLICATION EQ 'TELLER' THEN
    IF Y.APP EQ 'TELLER' THEN

* IF R.NEW(TT.TE.DR.CR.MARKER) EQ 'CREDIT' THEN
        IF EB.SystemTables.getRNew(TT.Contract.Teller.TeDrCrMarker) EQ 'CREDIT' THEN
* Y.DR.AC = R.NEW(TT.TE.ACCOUNT.2)
            Y.DR.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
*  Y.CR.AC = R.NEW(TT.TE.ACCOUNT.1)
            Y.CR.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
        END ELSE
* Y.DR.AC = R.NEW(TT.TE.ACCOUNT.1)
            Y.DR.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
* Y.CR.AC = R.NEW(TT.TE.ACCOUNT.2)
            Y.CR.AC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
        
        END


        EB.DataAccess.FRead(FN.AC,Y.DR.AC,DR.AC.REC,F.AC,AC.ERR)
* Y.DR.AC.CAT = DR.AC.REC<AC.CATEGORY>
        Y.DR.AC.CAT = DR.AC.REC<AC.AccountOpening.Account.Category>

        IF Y.DR.AC.CAT EQ 14081 AND Y.VERSION NE ',MBL.SDSA.LCY.CASHWDL.SUSP' THEN
*            ETEXT = "Use 'Cash Withdrawl-Suspense-ORG' from 'Teller Operations' Menu"
            EB.SystemTables.setEtext("Use 'Cash Withdrawl-Suspense-ORG' from 'Teller Operations' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()

        END

        IF Y.DR.AC.CAT EQ 15258 AND Y.VERSION NE ',MBL.SDSA.LCY.CASHWDL' THEN
* ETEXT = "Use 'Cash Withdrawl Sundry-ADJ' from 'Teller Operations' Menu"
            EB.SystemTables.setEtext("Use 'Cash Withdrawl Sundry-ADJ' from 'Teller Operations' Menu")
* CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END


        EB.DataAccess.FRead(FN.AC,Y.CR.AC,CR.AC.REC,F.AC,AC.ERR)
* Y.CR.AC.CAT = CR.AC.REC<AC.CATEGORY>
        Y.CR.AC.CAT = CR.AC.REC<AC.AccountOpening.Account.Category>

        IF Y.CR.AC.CAT EQ 14081 AND Y.VERSION NE ',MBL.SDSA.LCY.CASHIN.SUSP' THEN
*            ETEXT = "Use 'Cash Deposit Suspense-ADJ' from 'Teller Operations' Menu"
            EB.SystemTables.setEtext("Use 'Cash Deposit Suspense-ADJ' from 'Teller Operations' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.CR.AC.CAT EQ 15258 AND Y.VERSION NE ',MBL.SDSA.LCY.CASHIN' THEN
*            ETEXT = "Use 'Cash Deposit Sundry-ORG' from 'Teller Operations' Menu"
            EB.SystemTables.setEtext("Use 'Cash Deposit Sundry-ORG' from 'Teller Operations' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

***********************************************
        !Validation for Manual IBTA Issue Transactions:
***********************************************

*  Y.ENTRY.TYPE =  R.NEW(TT.TE.LOCAL.REF)<1,10>
        Y.ENTRY.TYPE =  EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)<1,10>

        IF Y.DR.AC.CAT EQ '12810' AND Y.ENTRY.TYPE NE 'RE' THEN
*            ETEXT = "Only IBTA Responding Entries are Allowed"
            EB.SystemTables.setEtext("Only IBTA Responding Entries are Allowed")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.CR.AC.CAT EQ '12810' AND Y.ENTRY.TYPE NE 'RE' THEN
*            ETEXT = "Only IBTA Responding Entries are Allowed"
            EB.SystemTables.setEtext("Only IBTA Responding Entries are Allowed")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

    END


*******************************
    !Validation for FT Application:
*******************************

    Y.USER.CO = EB.SystemTables.getIdCompany()

    IF Y.APP EQ 'FUNDS.TRANSFER' THEN
*        Y.DR.AC = R.NEW(FT.DEBIT.ACCT.NO)
        Y.DR.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
*        Y.CR.AC = R.NEW(FT.CREDIT.ACCT.NO)
        Y.CR.AC = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)


*******************NEW ADD BY BINOY ON 20210331 ***********
        IF Y.DR.AC[1,12] EQ 'BDT136920001' AND Y.VERSION NE ',STMU.SUSP.ORGINATING' THEN
*            ETEXT = "Use 'Interest TRF of Working Capital Stimulus Pack' from 'Maintain Loans' Menu"
            EB.SystemTables.setEtext("Use 'Interest TRF of Working Capital Stimulus Pack' from 'Maintain Loans' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.CR.AC[1,12] EQ 'BDT136920001' AND Y.VERSION NE ',STMU.SUSP.ADJ' THEN
*            ETEXT = "Use 'Interest ADJ of Working Capital Stimulus Pack' from 'Maintain Loans' Menu"
            EB.SystemTables.setEtext("Use 'Interest ADJ of Working Capital Stimulus Pack' from 'Maintain Loans' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END
****************NEW ADD BY BINOY
        EB.DataAccess.FRead(FN.AC,Y.DR.AC,DR.AC.REC,F.AC,AC.ERR)
* Y.DR.AC.CAT = DR.AC.REC<AC.CATEGORY>
        Y.DR.AC.CAT = DR.AC.REC<AC.AccountOpening.Account.Category>

        IF Y.DR.AC.CAT EQ 14081 AND Y.VERSION NE ',MBL.SUSP.ORG' THEN
*            ETEXT = "Use 'Suspense Originating' from 'Transfer' Menu"
            EB.SystemTables.setEtext("Use 'Suspense Originating' from 'Transfer' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.DR.AC.CAT EQ 15258 AND Y.VERSION NE ',MBL.SUNDRY.ADJ' THEN
*            ETEXT = "Use 'Sundry Adjustment' from 'Transfer' Menu"
            EB.SystemTables.setEtext("Use 'Sundry Adjustment' from 'Transfer' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.USER.CO NE "BD0010001" THEN

            IF Y.DR.AC.CAT EQ 17423 AND Y.VERSION NE ',INT.SUSP.ADJ' THEN
*                ETEXT = "Use 'Int Suspense Debit' from 'Maintain Interest Suspense' Menu"
                EB.SystemTables.setEtext("Use 'Int Suspense Debit' from 'Maintain Interest Suspense' Menu")
*                CALL STORE.END.ERROR
                EB.ErrorProcessing.StoreEndError()
            END
        END

        EB.DataAccess.FRead(FN.AC,Y.CR.AC,CR.AC.REC,F.AC,AC.ERR)
* Y.CR.AC.CAT = CR.AC.REC<AC.CATEGORY>
        Y.CR.AC.CAT = CR.AC.REC<AC.AccountOpening.Account.Category>

        IF Y.CR.AC.CAT EQ 14081 AND Y.VERSION NE ',MBL.SUSP.ADJ' THEN
*            ETEXT = "Use 'Suspense Adjustment' from 'Transfer' Menu"
            EB.SystemTables.setEtext("Use 'Suspense Adjustment' from 'Transfer' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.CR.AC.CAT EQ 15258 AND Y.VERSION NE ',MBL.SUNDRY.ORG' THEN
*            ETEXT = "Use 'Sundry Originating' from 'Transfer' Menu"
            EB.SystemTables.setEtext("Use 'Sundry Originating' from 'Transfer' Menu")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.USER.CO NE "BD0010001" THEN

            IF Y.CR.AC.CAT EQ 17423 AND Y.VERSION NE ',INT.SUSP.ORG' THEN
*                ETEXT = "Use 'Int Suspense Credit' from 'Maintain Interest Suspense' Menu"
                EB.SystemTables.setEtext("Use 'Int Suspense Credit' from 'Maintain Interest Suspense' Menu")
*                CALL STORE.END.ERROR
                EB.ErrorProcessing.StoreEndError()
            END

        END

***********************************************
        !Validation for Manual IBTA Issue Transactions:
***********************************************

* Y.ENTRY.TYPE = R.NEW(FT.LOCAL.REF)<1,18>
        Y.ENTRY.TYPE = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,18>

        IF Y.DR.AC.CAT EQ '12810' AND Y.ENTRY.TYPE NE 'RE' THEN
*            ETEXT = "Only IBTA Responding Entries are Allowed"
            EB.SystemTables.setEtext("Only IBTA Responding Entries are Allowed")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END

        IF Y.CR.AC.CAT EQ '12810' AND Y.ENTRY.TYPE NE 'RE' THEN
*            ETEXT = "Only IBTA Responding Entries are Allowed"
            EB.SystemTables.setEtext("Only IBTA Responding Entries are Allowed")
*            CALL STORE.END.ERROR
            EB.ErrorProcessing.StoreEndError()
        END
 
    END

**-----------------------------------------TRACER--------------------------------------------
*    WriteData = "SDSA WORKING":" ":Y.APP:" ":Y.VERSION
*    FileName = 'SHIBLI_SDSA.CHECK2.txt'
*    FilePath = 'DL.BP'
*    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
*    ELSE
*        CREATE FileOutput ELSE
*        END
*    END
*    WRITESEQ WriteData APPEND TO FileOutput ELSE
*        CLOSESEQ FileOutput
*    END
*    CLOSESEQ FileOutput
**-----------------------------------------TRACER END--------------------------------------------

RETURN
END
