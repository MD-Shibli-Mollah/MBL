* @ValidationCode : MjotODEyMDgzMjE3OkNwMTI1MjoxNjM0NDY3MjE3MjQ0OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 17 Oct 2021 16:40:17
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE CM.MBL.NOSTRO.TRANS.LIST(Y.ARRAY)
    
*  Modified by MD SHIBLI MOLLAH FDS -- on 13TH OCT 2021

*-----------------------------------------------------------------------------

    !PROGRAM NOSTRO.TRANS.LIST
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
  
* $INSERT T24.BP I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT T24.BP I_F.ACCOUNT.STATEMENT
    $USING ST.AccountStatement
*    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT T24.BP I_F.DRAWINGS
    $USING LC.Contract
*    $INSERT T24.BP I_F.STMT.ENT
    $USING AC.EntryCreation
*    $INSERT T24.BP I_F.COMPANY
    $USING ST.CompanyCreation
    $USING EB.DataAccess
    $USING EB.Reports

    GOSUB INIT
    GOSUB PROCESS
    !GOSUB WRITE.FILE
RETURN

INIT:

    FN.ACC='F.ACCOUNT'
    F.ACC=''
    EB.DataAccess.Opf(FN.ACC,F.ACC)

    FN.ACC.HIS='F.ACCOUNT$HIS'
    F.ACC.HIS=''
    EB.DataAccess.Opf(FN.ACC.HIS,F.ACC.HIS)


    FN.LCD='F.DRAWINGS'
    F.LCD=''
    EB.DataAccess.Opf(FN.LCD,F.LCD)

    FN.LCD.HIS='F.DRAWINGS$HIS'
    F.LCD.HIS=''
    EB.DataAccess.Opf(FN.LCD.HIS,F.LCD.HIS)


    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    EB.DataAccess.Opf(FN.FT,F.FT)

    FN.FT.HIS='F.FUNDS.TRANSFER$HIS'
    F.FT.HIS=''
    EB.DataAccess.Opf(FN.FT.HIS,F.FT.HIS)

    FN.ACCT.STMT='F.ACCT.STMT.PRINT'
    F.ACCT.STMT=''
    EB.DataAccess.Opf(FN.ACCT.STMT,F.ACCT.STMT)

    FN.STMT.PRNTD='F.STMT.PRINTED'
    F.STMT.PRNTD=''
    EB.DataAccess.Opf(FN.STMT.PRNTD,F.STMT.PRNTD)

    FN.STMT.ENT='F.STMT.ENTRY'
    F.STMT.ENT=''
    EB.DataAccess.Opf(FN.STMT.ENT,F.STMT.ENT)

    FN.ACCT.STEMNT='F.ACCOUNT.STATEMENT'
    F.ACCT.STEMNT=''
    EB.DataAccess.Opf(FN.ACCT.STEMNT,F.ACCT.STEMNT)

    FN.COMP='F.COMPANY'
    F.COMP=''
    EB.DataAccess.Opf(FN.COMP,F.COMP)

    FN.MNEMONIC='F.MNEMONIC.COMPANY'
    F.MNEMONIC=''
    EB.DataAccess.Opf(FN.MNEMONIC,F.MNEMONIC)

RETURN

PROCESS:
*

*    LOCATE "TRANS.DATE" IN D.FIELDS<1> SETTING POS THEN
*        Y.DATE=TRIM(D.RANGE.AND.VALUE<POS>)
*    END
    Y.DATE = EB.Reports.getEnqSelection()<4,1>

    !CRT" ENTER DATE................"
    !INPUT Y.DATE
* SEL.CMD = "SELECT ": FN.ACC :" WITH @ID LIKE '100150...'"
   
****R19 NOSTRO ACCOUNT STARTS WITH 1500 ..................***************
    SEL.CMD = "SELECT ": FN.ACC :" WITH @ID LIKE '1500...'"
   
    EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.REC,RRR.CODE)
    LOOP
        REMOVE Y.ACC.NO FROM SEL.LIST SETTING AC.POS
    WHILE Y.ACC.NO:AC.POS
        Y.STMT.PRINTED.ID=Y.ACC.NO:'-':Y.DATE

        EB.DataAccess.FRead(FN.STMT.PRNTD,Y.STMT.PRINTED.ID,PRINTED.REC,F.STMT.PRNTD,PRINTED.ERR)
        IF PRINTED.REC THEN

            Y.STMT.CNT= DCOUNT(PRINTED.REC,@FM)
            FOR I = 1 TO Y.STMT.CNT

                Y.STMT.ID=PRINTED.REC<I>

                EB.DataAccess.FRead(FN.STMT.ENT,Y.STMT.ID,STMT.REC,F.STMT.ENT,STMT.ENTRY.ERR)
*                Y.STMT.AC=STMT.REC<AC.STE.ACCOUNT.NUMBER>
                Y.STMT.AC=STMT.REC<AC.EntryCreation.StmtEntry.SteAccountNumber>
*                Y.LCY.AMT=STMT.REC<AC.STE.AMOUNT.LCY>
                Y.LCY.AMT=STMT.REC<AC.EntryCreation.StmtEntry.SteAmountLcy>
*                Y.SYSTEM.ID= STMT.REC<AC.STE.SYSTEM.ID>
                Y.SYSTEM.ID=STMT.REC<AC.EntryCreation.StmtEntry.SteSystemId>
*                Y.CURRENCY=STMT.REC<AC.STE.CURRENCY>
                Y.CURRENCY=STMT.REC<AC.EntryCreation.StmtEntry.SteCurrency>
*                Y.FCY.AMT=STMT.REC<AC.STE.AMOUNT.FCY>
                Y.FCY.AMT=STMT.REC<AC.EntryCreation.StmtEntry.SteAmountFcy>
                
                IF Y.FCY.AMT LT 0 THEN
                    Y.DEBIT.FCY=ABS(Y.FCY.AMT)
                END ELSE
                    Y.CREDIT.FCY=Y.FCY.AMT
                END
            
*                Y.RATE=STMT.REC<AC.STE.EXCHANGE.RATE>
                Y.RATE=STMT.REC<AC.EntryCreation.StmtEntry.SteExchangeRate>
*                Y.TRNS.REF=STMT.REC<AC.STE.TRANS.REFERENCE>
                Y.TRNS.REF=STMT.REC<AC.EntryCreation.StmtEntry.SteTransReference>

                Y.ID=FIELD(Y.TRNS.REF,'\',1)
                
                IF Y.SYSTEM.ID EQ 'FT' THEN
                    GOSUB FT.TRANS
                END ELSE
                    GOSUB LCD.TRANS
                END
                Y.MNEMONIC=FIELD(Y.TRNS.REF,'\',2)
*                Y.BOOK.DATE=STMT.REC<AC.STE.BOOKING.DATE>
                Y.BOOK.DATE=STMT.REC<AC.EntryCreation.StmtEntry.SteBookingDate>

                EB.DataAccess.FRead(FN.MNEMONIC,Y.MNEMONIC,MNEMONIC.REC,F.MNEMONIC,MNEMONIC.ERR)
                Y.COMP.ID = MNEMONIC.REC<ST.CompanyCreation.MnemonicCompany.AcMcoCompany>
                
                EB.DataAccess.FRead(FN.COMP,Y.COMP.ID,COMP.REC,F.COMP,COMP.ERR)
*   Y.COMP.NAME=COMP.REC<EB.COM.COMPANY.NAME>
                Y.COMP.NAME=COMP.REC<ST.CompanyCreation.Company.EbComCompanyName>
            
                IF Y.COMP.NAME EQ '' THEN
                    Y.COMP.NAME='Own Company'
                END

                !Y.ARRAY<-1> =Y.ACC.NO:'~':Y.STMT.ID:'~':Y.DEBIT.LCY:'~':Y.DEBIT.ACCT.NO:'~':Y.CREDIT.LCY:'~':Y.CREDIT.ACCT.NO:'~':Y.CONTRA.ACC:'~':Y.CURRENCY:'~':Y.DEBIT.FCY:'~':Y.CREDIT.FCY:'~':Y.RATE:'~':Y.TRNS.REF:'~':Y.BOOK.DATE:'~':Y.COMP.NAME
                Y.ARRAY<-1> =Y.CURRENCY:'*':Y.ACC.NO:'*':Y.DEBIT.LCY:'*':Y.CREDIT.LCY:'*':Y.CONTRA.ACC:'*':Y.ACCOUNT.TITLE:'*':Y.DEBIT.FCY:'*':Y.CREDIT.FCY:'*':Y.RATE:'*':Y.TRNS.REF:'*':Y.BOOK.DATE:'*':Y.COMP.NAME
                Y.ARRAY=SORT(Y.ARRAY)
                GOSUB CLEAR.DATA

            NEXT I

        END

    REPEAT
RETURN

FT.TRANS:


    EB.DataAccess.FRead(FN.FT,Y.ID,FT.REC,F.FT,FT.ERR)

    IF FT.REC THEN
*        Y.DEBIT.ACCT.NO=FT.REC<FT.DEBIT.ACCT.NO>
        Y.DEBIT.ACCT.NO=FT.REC<FT.Contract.FundsTransfer.DebitAcctNo>
*        Y.CREDIT.ACCT.NO=FT.REC<FT.CREDIT.ACCT.NO>
        Y.CREDIT.ACCT.NO=FT.REC<FT.Contract.FundsTransfer.CreditAcctNo>
    END ELSE
*  CALL EB.READ.HISTORY.REC(F.FT.HIS,Y.ID,FT.REC.HIS,FT.HIS.ERR)
    
        EB.DataAccess.ReadHistoryRec(F.FT.HIS,Y.ID,FT.REC.HIS,FT.HIS.ERR)
*        Y.DEBIT.ACCT.NO=FT.REC.HIS<FT.DEBIT.ACCT.NO>
        Y.DEBIT.ACCT.NO=FT.REC.HIS<FT.Contract.FundsTransfer.DebitAcctNo>
*        Y.CREDIT.ACCT.NO=FT.REC.HIS<FT.CREDIT.ACCT.NO>
        Y.CREDIT.ACCT.NO=FT.REC.HIS<FT.Contract.FundsTransfer.CreditAcctNo>
    END

    IF Y.LCY.AMT LT 0 THEN
        Y.DEBIT.LCY=ABS(Y.LCY.AMT)
        Y.CONTRA.ACC=Y.CREDIT.ACCT.NO
    END ELSE
        Y.CREDIT.LCY=Y.LCY.AMT
        Y.CONTRA.ACC=Y.DEBIT.ACCT.NO
    END
    GOSUB AC.INFO
RETURN

LCD.TRANS:

    EB.DataAccess.FRead(FN.LCD,Y.ID,LCD.REC,F.LCD,LCD.ERR)
    IF LCD.REC THEN
*        Y.DRAWDOWN.ACCOUNT=LCD.REC<TF.DR.DRAWDOWN.ACCOUNT>
        Y.DRAWDOWN.ACCOUNT=LCD.REC<LC.Contract.Drawings.TfDrDrawdownAccount>
*        Y.PAYMENT.ACCOUNT=LCD.REC<TF.DR.PAYMENT.ACCOUNT>
        Y.PAYMENT.ACCOUNT=LCD.REC<LC.Contract.Drawings.TfDrPaymentAccount>
        Y.DEBIT.ACCT.NO=Y.DRAWDOWN.ACCOUNT
        Y.CREDIT.ACCT.NO=Y.PAYMENT.ACCOUNT
    END ELSE
* CALL EB.READ.HISTORY.REC(F.LCD.HIS,Y.ID,LCD.REC.HIS,LCD.HIS.ERR)
        EB.DataAccess.ReadHistoryRec(F.LCD.HIS,Y.ID,LCD.REC.HIS,LCD.HIS.ERR)
*        Y.DRAWDOWN.ACCOUNT=LCD.REC.HIS<TF.DR.DRAWDOWN.ACCOUNT>
        Y.DRAWDOWN.ACCOUNT=LCD.REC.HIS<LC.Contract.Drawings.TfDrDrawdownAccount>
        Y.PAYMENT.ACCOUNT=LCD.REC.HIS<LC.Contract.Drawings.TfDrPaymentAccount>
        Y.DEBIT.ACCT.NO=Y.DRAWDOWN.ACCOUNT
        Y.CREDIT.ACCT.NO=Y.PAYMENT.ACCOUNT
    END
    IF Y.LCY.AMT LT 0 THEN
        Y.DEBIT.LCY=ABS(Y.LCY.AMT)
        Y.CONTRA.ACC=Y.PAYMENT.ACCOUNT
    END ELSE
        Y.CREDIT.LCY=Y.LCY.AMT
        Y.CONTRA.ACC=Y.DRAWDOWN.ACCOUNT
    END
    GOSUB AC.INFO
RETURN

AC.INFO:
    !*
    Y.ACCOUNT.ID=Y.CONTRA.ACC
    EB.DataAccess.FRead(FN.ACC,Y.ACCOUNT.ID,AC.REC,F.ACC,AC.ERR)
    IF AC.REC THEN
* Y.ACCOUNT.TITLE=AC.REC<AC.ACCOUNT.TITLE.1>
        Y.ACCOUNT.TITLE=AC.REC<AC.AccountOpening.Account.AccountTitleOne>
    END ELSE
* CALL EB.READ.HISTORY.REC(F.ACC.HIS,Y.ACCOUNT.ID,ACC.REC.HIS,ACC.HIS.ERR)
        EB.DataAccess.ReadHistoryRec(F.ACC.HIS,Y.ACCOUNT.ID,ACC.REC.HIS,ACC.HIS.ERR)
*  Y.ACCOUNT.TITLE=ACC.REC.HIS<AC.ACCOUNT.TITLE.1>
        Y.ACCOUNT.TITLE=AC.REC<AC.AccountOpening.Account.AccountTitleOne>
    END
RETURN

CLEAR.DATA:
    !Y.ACC.NO=''
    Y.DEBIT.LCY=''
    Y.CREDIT.LCY=''

    Y.CURRENCY=''
    Y.DEBIT.FCY=''
    Y.CREDIT.FCY=''
    Y.RATE=''
    Y.TRNS.REF=''
    Y.BOOK.DATE=''
    Y.LCY.AMT=''
    Y.FCY.AMT=''
    Y.CREDIT.ACCT.NO=''
    Y.DEBIT.ACCT.NO=''
    Y.PAYMENT.ACCOUNT=''
    Y.DRAWDOWN.ACCOUNT=''
    Y.CONTRA.ACC=''
    Y.COMP.ID = ''
    Y.MNEMONIC = ''
RETURN
END
!WRITE.FILE:
!Y.DIR = './MBL.DATA.EXT'
!LINE.FEED = CHARX(10)
!Y.FILENAME = 'NOSTRO.TRANS.LIST.DATA'
!OPENSEQ Y.DIR,Y.FILENAME TO C.OUT ELSE
!CREATE C.OUT ELSE
!CRT "UNABLE TO CREATE FILE"
!END
!END
!CONVERT FM TO LINE.FEED IN Y.ARRAY

!WRITESEQ Y.ARRAY TO C.OUT ELSE

!CRT "UNABLE TO WRITE"
!CLOSESEQ C.OUT
!END
!RETURN
