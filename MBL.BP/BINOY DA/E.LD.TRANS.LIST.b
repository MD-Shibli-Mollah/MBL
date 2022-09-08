* @ValidationCode : MjoxNTk3NjYzMjczOkNwMTI1MjoxNjA3MjUwODI4NDA2OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 06 Dec 2020 16:33:48
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
*!    PROGRAM E.LD.TRANS.LIST
SUBROUTINE E.LD.TRANS.LIST(ARRAY)
*
*    $INSERT I_COMMON
*    $INSERT I_EQUATE
*    $INSERT I_ENQUIRY.COMMON
*
**    $INSERT T24.BP I_F.LD.LOANS.AND.DEPOSITS
*    $USING EB.Reports
*    $USING EB.SystemTables
*    $USING AC.AccountOpening
*    $USING EB.DataAccess
*    $USING AA.Framework
*    $USING RE.ConBalanceUpdates
*    $USING AA.Account
*    $USING AA.Limit
*    $USING ST.Customer
*    $USING LI.Config
*    $USING AA.Interest
*    $USING ST.CompanyCreation
*    $USING AA.PaymentSchedule
*    $USING AA.ProductManagement
*    $USING ST.Config
*    $USING AA.TermAmount
*    $USING EB.Updates
*    $USING AC.EntryCreation
*
**    $INSERT T24.BP I_F.PD.PAYMENT.DUE
**    $INSERT T24.BP I_F.RE.CONSOL.SPEC.ENTRY
**    $INSERT T24.BP I_F.CATEG.ENTRY
**    $INSERT T24.BP I_F.STMT.ENTRY
**    $INSERT T24.BP I_F.EB.CONTRACT.BALANCES
**    $INSERT T24.BP I_F.EB.CONTRACT.ENTRIES
**    $INSERT T24.BP I_F.COMPANY
*
*    GOSUB INTT
*    GOSUB OPNFILE
*    GOSUB PROCESS
*RETURN
*
*INTT:
*
*    FN.COM = 'F.COMPANY'
*    F.COM = ''
*
*    FN.STMT = 'F.STMT.ENTRY'
*    F.STMT = ''
*
*    FN.ECE = 'F.EB.CONTRACT.ENTRIES'
*    F.ECE = ''
*
*    FN.ECB = 'F.EB.CONTRACT.BALANCES'
*    F.ECB = ''
*
*    FN.SPEC = 'F.RE.CONSOL.SPEC.ENTRY'
*    F.SPEC = ''
*
*    FN.CATEG = 'F.CATEG.ENTRY'
*    F.CATEG = ''
*
**    FN.LD = 'F.LD.LOANS.AND.DEPOSITS'
**    F.LD = ''
*
*    Y.LD.ID =''
*
*    !DEBUG
**    LOCATE "LOAN.ID" IN D.FIELDS<1> SETTING POS THEN
**        Y.LD.ID=TRIM(D.RANGE.AND.VALUE<POS>)
**    END
*
*    LOCATE "ARR.ID" IN EB.Reports.getEnqSelection()<2,1> SETTING ARR.POS THEN
*        Y.ARR.ID=EB.Reports.getEnqSelection()<4,ARR.POS>
*    END
*
**  Y.ACC.ID = Y.LD.ID
**  Y.LD.NO = ''
*    ARRAY = ''
*
*RETURN
*
*OPNFILE:
*
*    EB.DataAccess.Opf(FN.STMT,F.STMT)
*    EB.DataAccess.Opf(FN.ECB,F.ECB)
*    EB.DataAccess.Opf(FN.CATEG,F.CATEG)
*    EB.DataAccess.Opf(FN.SPEC,F.SPEC)
*RETURN
*
*PROCESS:
*    !DEBUG
*    !IF Y.LD.ID MATCHES 'LD...' THEN
*    !  Y.PD.ID='PD':Y.LD.ID
*    !END
*    !IF Y.LD.ID MATCHES 'PD...' THEN
*    !   Y.PD.ID=Y.LD.ID
*    !   Y.LD.ID =Y.LD.ID[3,12]
*    !END
*
*    EB.DataAccess.FRead(FN.ECB,Y.LD.ID,R.ECB,F.ECB,ERR.ECB)
*
*    IF R.ECB THEN
*
**        Y.STMT.CNT   = DCOUNT(R.ECB<ECB.STMT.ENT.IDS>,VM)
*        Y.STMT.CNT   = DCOUNT(R.ECB<RE.ConBalanceUpdates.EbContractBalances.EcbStmtEntIds>,VM)
**    Y.STMT.SPLIT = R.ECB<ECB.STMT.SPLIT.ID>
*        Y.STMT.SPLIT = R.ECB<RE.ConBalanceUpdates.EbContractBalances.EcbStmtSplitId>
**----------------------------------------------------------------------------------
** Y.SPEC.CNT   = DCOUNT(R.ECB<ECB.CONSOL.ENT.IDS>,VM)
*        Y.SPEC.CNT   = DCOUNT(R.ECB<RE.ConBalanceUpdates.EbContractBalances.EcbConsolEntIds>,VM)
**Y.SPEC.SPLIT = R.ECB<ECB.CONSOL.SPLIT.ID>
*
*        Y.SPEC.SPLIT = R.ECB<RE.ConBalanceUpdates.EbContractBalances.EcbConsolSplitId>
** Y.CATEG.CNT  = DCOUNT(R.ECB<ECB.CATEG.ENT.IDS>,VM)
*        Y.CATEG.CNT  = DCOUNT(R.ECB<RE.ConBalanceUpdates.EbContractBalances.EcbCategEntIds>,VM)
**Y.CATEG.SPLIT= R.ECB<ECB.CATEG.SPLIT.ID>
*        Y.CATEG.SPLIT= R.ECB<RE.ConBalanceUpdates.EbContractBalances.EcbCategSplitId>
*        ! For STMT
*        ! ==========
*        FOR I = 1 TO Y.STMT.CNT
**Y.STMT.ID = R.ECB<ECB.STMT.ENT.IDS,I>
*            Y.STMT.ID = R.ECB<RE.ConBalanceUpdates.EbContractBalances.EcbStmtEntIds,I>
*            Y.STMT.ID = Y.STMT.ID[1,22]
*
*            GOSUB STMT.ENTRY
*            GOSUB RESULT
*        NEXT I
*
*        IF Y.STMT.SPLIT GT 0 THEN
*            FOR X = 1 TO Y.STMT.SPLIT
*                Y.STMT.SP.ID =Y.LD.ID:'*S*':X
*                EB.DataAccess.FRead(FN.ECE,Y.STMT.SP.ID,R.ECE,F.ECE,ERR.ECE)
*                Y.STMT.CNT2 = DCOUNT(R.ECE,FM)
*                FOR Q =1 TO Y.STMT.CNT2
*                    Y.CONTRACT.ID=FIELD(R.ECE<Q,1>,'/',1)
*                    Y.STMT.ID=Y.CONTRACT.ID
*
*                    GOSUB STMT.ENTRY
*                    GOSUB RESULT
*                NEXT Q
*            NEXT X
*        END
*
*        ! For SPEC
*        ! ==========
*        SEL.CMD.SUS = "SELECT FBNK.RE.CONSOL.SPEC.ENTRY WITH OUR.REFERENCE EQ ":Y.LD.ID
*        EB.DataAccess.Readlist(SEL.CMD.SUS,SEL.LIST.SUS,'',NO.OF.REC,RET.CODE)
*        LOOP
*            REMOVE Y.SPEC.ID FROM SEL.LIST.SUS SETTING POS
*        WHILE Y.SPEC.ID:POS
*
*            GOSUB SPEC.ENTRY
*            GOSUB RESULT
*        REPEAT
*
*        !       FOR J = 1 TO Y.SPEC.CNT
*        !           Y.SPEC.ID = R.ECB<ECB.CONSOL.ENT.IDS,J>
*        !           Y.SPEC.ID = Y.SPEC.ID[1,22]
*        !           GOSUB SPEC.ENTRY
*        !           GOSUB RESULT
*        !
*        !       NEXT J
*
*        !       IF Y.SPEC.SPLIT GT 0 THEN
*        !           FOR X = 1 TO Y.SPEC.SPLIT
*        !               Y.SPEC.SP.ID =Y.LD.ID:'*R*':X
*        !               CALL F.READ(FN.ECE,Y.SPEC.SP.ID,R.ECE,F.ECE,ERR.ECE)
*        !               Y.SPEC.CNT2 = DCOUNT(R.ECE,FM)
*        !               FOR P=1 TO Y.SPEC.CNT2
*        !                   Y.CONTRACT.ID=FIELD(R.ECE<P,1>,'/',1)
*        !                   Y.SPEC.ID=Y.CONTRACT.ID
*        !
*        !                   GOSUB SPEC.ENTRY
*        !                   GOSUB RESULT
*        !               NEXT P
*        !           NEXT X
*        !       END
*
*        ! For CATEG
*        ! ==========
*        FOR K = 1 TO Y.CATEG.CNT
**Y.CATEG.ID = R.ECB<ECB.CATEG.ENT.IDS,K>
*            Y.CATEG.ID = R.ECB<RE.ConBalanceUpdates.EbContractBalances.EcbCategEntIds,K>
*            Y.CATEG.ID = Y.CATEG.ID[1,22]
*
*            GOSUB CATEG.ENTRY
*            GOSUB RESULT
*        NEXT K
*
*        IF Y.CATEG.SPLIT GT 0 THEN
*            FOR Z = 1 TO Y.CATEG.SPLIT
*                Y.CATEG.SP.ID =Y.LD.ID:'*C*':Z
*                EB.DataAccess.FRead(FN.ECE,Y.CATEG.SP.ID,R.ECE,F.ECE,ERR.ECE)
*                Y.CATEG.CNT2 = DCOUNT(R.ECE,FM)
*                FOR P = 1 TO Y.CATEG.CNT2
*                    Y.CONTRACT.ID=FIELD(R.ECE<P,1>,'/',1)
*                    Y.CATEG.ID=Y.CONTRACT.ID
*
*                    GOSUB CATEG.ENTRY
*                    GOSUB RESULT
*                NEXT P
*            NEXT Z
*        END
*
*    END
*    !END
*
*    !   LIN.FEED = CHARX(10)
*    !   CONVERT FM TO LIN.FEED IN ARRAY
*    !   CRT ARRAY
*RETURN
*
*STMT.ENTRY:
*
*    EB.DataAccess.FRead(FN.STMT,Y.STMT.ID,R.STMT,F.STMT,ERR.STMT)
*    Y.ENTRY.ID =Y.STMT.ID
** Y.CO.CODE  =R.STMT<AC.STE.COMPANY.CODE>
*    Y.CO.CODE  =R.STMT<AC.EntryCreation.StmtEntry.SteCompanyCode>
*    EB.DataAccess.FRead(FN.COM,Y.CO.CODE,R.COM,F.COM,ERR.COM)
**Y.CO.CODE  = R.COM<EB.COM.COMPANY.NAME>
*    Y.CO.CODE  = R.COM<ST.CompanyCreation.Company.EbComCompanyName>
*
** Y.ACCOUNT  =R.STMT<AC.STE.ACCOUNT.NUMBER>
*    Y.ACCOUNT  =R.STMT<AC.EntryCreation.StmtEntry.SteAccountNumber>
**Y.AMOUNT   =R.STMT<AC.STE.AMOUNT.LCY>
*    Y.AMOUNT   =R.STMT<AC.EntryCreation.StmtEntry.SteAmountLcy>
**Y.TRN.CODE =R.STMT<AC.STE.TRANSACTION.CODE>
*    Y.TRN.CODE =R.STMT<AC.EntryCreation.StmtEntry.SteTransactionCode>
**Y.CONSOL.KEY =R.STMT<AC.STE.CONSOL.KEY>
*    Y.CONSOL.KEY =R.STMT<AC.EntryCreation.StmtEntry.SteConsolKey>
*
**Y.ASSET.TYPE =R.STMT<AC.STE.CRF.TYPE>
*    Y.ASSET.TYPE =R.STMT<AC.EntryCreation.StmtEntry.SteCrfType>
**Y.BOOKING.DATE =R.STMT<AC.STE.BOOKING.DATE>
*    Y.BOOKING.DATE =R.STMT<AC.EntryCreation.StmtEntry.SteBookingDate>
** Y.SYSTEM.ID =R.STMT<AC.STE.SYSTEM.ID>
*    Y.SYSTEM.ID =R.STMT<AC.EntryCreation.StmtEntry.SteSystemId>
**Y.INPUTTER =FIELD(R.STMT<AC.STE.INPUTTER>,'_',2)
*    Y.INPUTTER =FIELD(R.STMT<AC.EntryCreation.StmtEntry.SteInputter>,'_',2)
*RETURN
*
*SPEC.ENTRY:
*
*    EB.DataAccess.FRead(FN.SPEC,Y.SPEC.ID,R.SPEC,F.SPEC,ERR.SPEC)
*    Y.ENTRY.ID =Y.SPEC.ID
*    Y.CO.CODE  =R.SPEC<RE.CSE.COMPANY.CODE>
*    CALL F.READ(FN.COM,Y.CO.CODE,R.COM,F.COM,ERR.COM)
*    Y.CO.CODE  = R.COM<EB.COM.COMPANY.NAME>
*    Y.ACCOUNT  = R.SPEC<RE.CSE.DEAL.NUMBER>
*    Y.AMOUNT   =R.SPEC<RE.CSE.AMOUNT.LCY>
*    Y.TRN.CODE =R.SPEC<RE.CSE.TRANSACTION.CODE>
*    Y.CONSOL.KEY =R.SPEC<RE.CSE.CONSOL.KEY.TYPE>
*    !    Y.ASSET.TYPE =RIGHT(Y.CONSOL.KEY,6)
*    Y.ASSET.TYPE =FIELD(Y.CONSOL.KEY,'.',18)
*    Y.BOOKING.DATE =R.SPEC<RE.CSE.BOOKING.DATE>
*    Y.SYSTEM.ID =R.SPEC<RE.CSE.SYSTEM.ID>
*    Y.INPUTTER =FIELD(R.SPEC<RE.CSE.INPUTTER>,'_',2)
*RETURN
*
*CATEG.ENTRY:
*
*    CALL F.READ(FN.CATEG,Y.CATEG.ID,R.CATEG,F.CATEG,ERR.CATEG)
*    Y.ENTRY.ID =Y.CATEG.ID
*    Y.CO.CODE  =R.CATEG<AC.CAT.COMPANY.CODE>
*    CALL F.READ(FN.COM,Y.CO.CODE,R.COM,F.COM,ERR.COM)
*    Y.CO.CODE  = R.COM<EB.COM.COMPANY.NAME>
*    Y.ACCOUNT  = R.CATEG<AC.CAT.PL.CATEGORY>
*    Y.AMOUNT   =R.CATEG<AC.CAT.AMOUNT.LCY>
*    Y.TRN.CODE =R.CATEG<AC.CAT.TRANSACTION.CODE>
*    Y.CONSOL.KEY =R.CATEG<AC.CAT.CONSOL.KEY>
*    Y.ASSET.TYPE =R.CATEG<AC.CAT.CRF.TYPE>
*    Y.BOOKING.DATE =R.CATEG<AC.CAT.BOOKING.DATE>
*    Y.SYSTEM.ID =R.CATEG<AC.CAT.SYSTEM.ID>
*    Y.INPUTTER =FIELD(R.CATEG<AC.CAT.INPUTTER>,'_',2)
*RETURN
*
*RESULT:
*    ARRAY<-1> = Y.ENTRY.ID:'*':Y.LD.ID:'*':Y.CO.CODE:'*':Y.ACCOUNT:'*':Y.AMOUNT:'*':Y.TRN.CODE:'*':Y.CONSOL.KEY:'*':Y.ASSET.TYPE:'*':Y.BOOKING.DATE:'*':Y.SYSTEM.ID:'*':Y.INPUTTER
*    ARRAY=SORT(ARRAY)
*    Y.CO.CODE = ''
*    Y.AMOUNT  = ''
*    Y.TRN.CODE = ''
*    Y.CONSOL.KEY = ''
*    Y.ASSET.TYPE = ''
*    Y.BOOKING.DATE = ''
*    Y.SYSTEM.ID = ''
*    Y.INPUTTER = ''
*RETURN
*
*END
