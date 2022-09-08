* @ValidationCode : MjoxOTgzODE3MTMzOkNwMTI1MjoxNjA3NTIyNDQ4ODg1OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 09 Dec 2020 20:00:48
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

* Modification History :
*-----------------------------------------------------------------------------
*  Retrofitted by MD SHIBLI MOLLAH FDS -- on 9TH DEC 2020
*-----------------------------------------------------------------------------

SUBROUTINE TT.A.TRAN.AC.SMS.OUT

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
*    $INSERT T24.BP I_F.ACCOUNT
    $USING AC.AccountOpening
*    $INSERT T24.BP  I_F.TELLER
    $USING TT.Contract
*    $INSERT T24.BP I_F.FUNDS.TRANSFER
    $USING FT.Contract
*    $INSERT T24.BP I_F.CUSTOMER
    $USING ST.Customer
    $USING EB.DataAccess
    $USING EB.SystemTables

* Y.CHK.AMOUNT = R.NEW(TT.TE.AMOUNT.LOCAL.1)
    Y.CHK.AMOUNT = EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne)


    !IF V$FUNCTION NE "A" OR Y.CHK.AMOUNT LT 5000 THEN RETURN
    IF V$FUNCTION NE "A" THEN RETURN


* Please add all category in CAT.LIST which will propagate through the entire subroutine.
    CAT.LIST='6000':@FM:'6001':@FM:'6002':@FM:'6003':@FM:'6004':@FM:'6005':@FM:'6006':@FM:'6007':@FM:'6008':@FM:'6010':@FM:'6011':@FM:'6012':@FM:'6013':@FM:'6014':@FM:'6030':@FM:'6032':@FM:'6033'
    CAT.LIST=CAT.LIST:@FM:'1001':@FM:'1002':@FM:'1003':@FM:'1004':@FM:'1005'
    CAT.LIST=CAT.LIST:@FM:'1934':@FM:'1990':@FM:'1936':@FM:'1947':@FM:'1945':@FM:'1941':@FM:'1943':@FM:'1946':@FM:'1944':@FM:'1940':@FM:'1942':@FM:'1921'
    CAT.LIST=CAT.LIST:@FM:'1922':@FM:'1924':@FM:'1935':@FM:'1925':@FM:'1933':@FM:'1981'
    CAT.LIST=CAT.LIST:@FM:'1928':@FM:'1964':@FM:'1927':@FM:'1931':@FM:'1932':@FM:'1926':@FM:'1948':@FM:'1965':@FM:'6031'


    !AC.BR ='BD0010111':@FM:'BD0010120':@FM:'BD0010148':@FM:'BD0010314':@FM:'BD0010101':@FM:'BD0010113':@FM:'BD0010115':@FM:'BD0010140':@FM:'BD0010121'
    !AC.BR ='BD0010001':@FM:'BD0010002':@FM:'BD0010101':@FM:'BD0010102':@FM:'BD0010103':@FM:'BD0010104':@FM:'BD0010105':@FM:'BD0010106':@FM:'BD0010107':@FM:'BD0010108':@FM:'BD0010109':@FM:'BD0010110':@FM:'BD0010111':@FM:'BD0010112':@FM:'BD0010113':@FM:'BD0010114':@FM:'BD0010115':@FM:'BD0010116':@FM:'BD0010117':@FM:'BD0010118':@FM:'BD0010119':@FM:'BD0010120':@FM:'BD0010121':@FM:'BD0010122':@FM:'BD0010123':@FM:'BD0010124':@FM:'BD0010125':@FM:'BD0010126':@FM:'BD0010127':@FM:'BD0010128':@FM:'BD0010129':@FM:'BD0010130':@FM:'BD0010131':@FM:'BD0010132':@FM:'BD0010133':@FM:'BD0010134':@FM:'BD0010135':@FM:'BD0010136':@FM:'BD0010137':@FM:'BD0010138':@FM:'BD0010139':@FM:'BD0010140':@FM:'BD0010141':@FM:'BD0010142':@FM:'BD0010143':@FM:'BD0010144':@FM:'BD0010145':@FM:'BD0010146':@FM:'BD0010147':@FM:'BD0010148':@FM:'BD0010149':@FM:'BD0010150':@FM:'BD0010154':@FM:'BD0010155':@FM:'BD0010156':@FM:'BD0010157':@FM:'BD0010158':@FM:'BD0010159':@FM:'BD0010160':@FM:'BD0010303'

    AC.BR ='BD0010350':@FM:'BD0010351':@FM:'BD0010352':@FM:'BD0010353'
    AC.BR=AC.BR:@FM:'BD0010101':@FM:'BD0010116':@FM:'BD0010129':@FM:'BD0010115'

    FN.AC = 'F.ACCOUNT'
    F.AC = ''
    EB.DataAccess.Opf(FN.AC,F.AC)

    FN.CUS = 'F.CUSTOMER'
    F.CUS = ''
    EB.DataAccess.Opf(FN.CUS,F.CUS)

    R.CR.AC.REC = ''
    R.DR.AC.REC = ''
    Y.CR.AC.ERR = ''
    Y.DR.AC.ERR = ''
    Y.CR.AC.ID = ''
    Y.DR.AC.ID = ''
    Y.CR.SMS.OUT = ''
    Y.DR.SMS.OUT = ''
    Y.CR.FILE.NAME = ''
    Y.DR.FILE.NAME = ''
    TOT.DEBIT1.AMT=''

*    Y.TT.ID = ID.NEW
    Y.TT.ID = EB.SystemTables.getIdNew()
    R.TT.REC = ''
    Y.TT.ERR = ''
*   Y.TODAY = TODAY
    Y.TODAY = EB.SystemTables.getToday()
    TIME.ST = TIMEDATE()
    Y.DATE.TIME = Y.TODAY:'_':TIME.ST[1,2]:TIME.ST[4,2]:TIME.ST[7,2]
    Y.DT.TM = Y.TODAY:' ':TIME.ST[1,2]:TIME.ST[4,2]


    !*************Teller Transaction Code 5,10,14*********************
    !***Cash WithdrawalCHQ through TT=5
    !***Cash Deposit through TT=10
    !***Cash Withdrawal through TT=14
    !***Online Cash Deposit through TT=35
    !***Online Cash Withdrawal TT=36
    !***Inward Clearing TT=44


************
    !TT.PROCESS
************

* Y.VAL = R.NEW(TT.TE.TRANSACTION.CODE)
    Y.VAL = EB.SystemTables.getRNew(TT.Contract.Teller.TeTransactionCode)


    BEGIN CASE
        CASE Y.VAL EQ 10
            GOSUB TT.CRAC.FILE.GEN

        CASE Y.VAL EQ 5

            GOSUB TT.CASHWDL.AC2.FILE.GEN

        CASE Y.VAL EQ 14

            GOSUB TT.CASHWDL.AC1.FILE.GEN

        CASE Y.VAL EQ 35
            GOSUB TT.CRAC.FILE.GEN

        CASE Y.VAL EQ 36
            GOSUB TT.CASHWDL.AC1.FILE.GEN

        CASE Y.VAL EQ 44
            GOSUB TT.CASHWDL.AC2.FILE.GEN


    END CASE
RETURN


TT.CRAC.FILE.GEN:
******************


* Y.CR.AC.ID = R.NEW(TT.TE.ACCOUNT.2)
    Y.CR.AC.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
*   Y.CR.CUS.ID = R.NEW(TT.TE.CUSTOMER.2)
    Y.CR.CUS.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeCustomerTwo)
    EB.DataAccess.FRead(FN.CUS,Y.CR.CUS.ID,R.TT.CR.CUS.REC,F.CUS,Y.CUS.ERR)
* Y.TT.CUS.CELL.CR = R.TT.CR.CUS.REC<EB.CUS.SMS.1>
    Y.TT.CUS.CELL.CR = R.TT.CR.CUS.REC<ST.Customer.Customer.EbCusSmsOne>
    CONVERT VM TO '~' IN Y.TT.CUS.CELL.CR

    EB.DataAccess.FRead(FN.AC,Y.CR.AC.ID,R.CR.AC.REC,F.AC,Y.CR.AC.ERR)
* Y.CR.AC.BR = R.CR.AC.REC<AC.CO.CODE>
    Y.CR.AC.BR = R.CR.AC.REC<AC.AccountOpening.Account.CoCode>

    LOCATE Y.CR.AC.BR IN AC.BR SETTING Y.CR.AC.BR.POS THEN

        Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SMS.OUT'
    END

    ELSE
        Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SSL.SMS.OUT/cbsfile'
    END

    !Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SSL.SMS.OUT/cbsfile'

    OPEN Y.SMS.BP TO F.SMS.OUT ELSE
        RETURN
    END

*    LOCATE R.CR.AC.REC<AC.CATEGORY> IN CAT.LIST SETTING Y.POS THEN
    LOCATE R.CR.AC.REC<AC.AccountOpening.Account.Category> IN CAT.LIST SETTING Y.POS THEN

*        Y.CR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':R.NEW(TT.TE.NARRATIVE.2):'#':R.NEW(TT.TE.ACCOUNT.2):'#':'CR':'#':R.NEW(TT.TE.AMOUNT.LOCAL.1):'#':Y.TT.CUS.CELL.CR:'#':R.CR.AC.REC<AC.ONLINE.ACTUAL.BAL>:'#':Y.CR.CUS.ID:'#':R.CR.AC.REC<AC.CATEGORY>:'#':R.NEW(TT.TE.CO.CODE)
        Y.CR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo):'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo):'#':'DR':'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne):'#':Y.TT.CUS.CELL.DR:'#':R.DR.AC.REC<AC.AccountOpening.Account.OnlineActualBal>:'#':Y.DR.CUS.ID:'#':R.DR.AC.REC<AC.AccountOpening.Account.Category>:'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeCoCode)
        Y.CR.FILE.NAME = 'SMS_':Y.TT.ID:".":'CR':'.':Y.DATE.TIME

        WRITE Y.CR.SMS.OUT TO F.SMS.OUT,Y.CR.FILE.NAME
    END
RETURN


!TT.CASHWDL.NOCHQ.FILE.GEN:
*********************

TT.CASHWDL.AC2.FILE.GEN:
**************************



*  Y.DR.AC.ID = R.NEW(TT.TE.ACCOUNT.2)
    Y.DR.AC.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
*  Y.DR.CUS.ID=R.NEW(TT.TE.CUSTOMER.2)
    Y.DR.CUS.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeCustomerOne)

    EB.DataAccess.FRead(FN.CUS,Y.DR.CUS.ID,R.TT.DR.CUS.REC,F.CUS,Y.CUS.ERR)
* Y.TT.CUS.CELL.DR=R.TT.DR.CUS.REC<EB.CUS.SMS.1>
    Y.TT.CUS.CELL.DR=R.TT.DR.CUS.REC<ST.Customer.Customer.EbCusSmsOne>
    CONVERT VM TO '~' IN Y.TT.CUS.CELL.DR

    EB.DataAccess.FRead(FN.AC,Y.DR.AC.ID,R.DR.AC.REC,F.AC,Y.DR.AC.ERR)
* Y.DR.AC.BR = R.DR.AC.REC<AC.CO.CODE>
    Y.DR.AC.BR = R.DR.AC.REC<AC.AccountOpening.Account.CoCode>

    LOCATE Y.DR.AC.BR IN AC.BR SETTING Y.DR.AC.BR.POS THEN

        Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SMS.OUT'
    END

    ELSE
        Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SSL.SMS.OUT/cbsfile'
    END

    !Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SSL.SMS.OUT/cbsfile'

    OPEN Y.SMS.BP TO F.SMS.OUT ELSE
        RETURN
    END


* LOCATE R.DR.AC.REC<AC.CATEGORY> IN CAT.LIST SETTING Y.POS THEN
    LOCATE R.DR.AC.REC<AC.AccountOpening.Account.Category> IN CAT.LIST SETTING Y.POS THEN

*  Y.DR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':R.NEW(TT.TE.NARRATIVE.2):'#':R.NEW(TT.TE.ACCOUNT.2):'#':'DR':'#':R.NEW(TT.TE.AMOUNT.LOCAL.1):'#':Y.TT.CUS.CELL.DR:'#':R.DR.AC.REC<AC.ONLINE.ACTUAL.BAL>:'#':Y.DR.CUS.ID:'#':R.DR.AC.REC<AC.CATEGORY>:'#':R.NEW(TT.TE.CO.CODE)
        Y.DR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo):'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo):'#':'DR':'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne):'#':Y.TT.CUS.CELL.DR:'#':R.DR.AC.REC<AC.AccountOpening.Account.OnlineActualBal>:'#':Y.DR.CUS.ID:'#':R.DR.AC.REC<AC.AccountOpening.Account.Category>:'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeCoCode)
       
        Y.DR.FILE.NAME = 'SMS_':Y.TT.ID:".":'DR':'.':Y.DATE.TIME

        WRITE Y.DR.SMS.OUT TO F.SMS.OUT,Y.DR.FILE.NAME

    END
RETURN

!TT.CASHWDL.CHQ.FILE.GEN:
************************

TT.CASHWDL.AC1.FILE.GEN:
************************

*    Y.DR.AC.ID = R.NEW(TT.TE.ACCOUNT.1)
    Y.DR.AC.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne)
*  Y.DR.CUS.ID=R.NEW(TT.TE.CUSTOMER.1)
    Y.DR.CUS.ID = EB.SystemTables.getRNew(TT.Contract.Teller.TeCustomerOne)

    EB.DataAccess.FRead(FN.CUS,Y.DR.CUS.ID,R.TT.DR.CUS.REC,F.CUS,Y.CUS.ERR)
*  Y.TT.CUS.CELL.DR=R.TT.DR.CUS.REC<EB.CUS.SMS.1>
    Y.TT.CUS.CELL.DR = R.TT.DR.CUS.REC<ST.Customer.Customer.EbCusSmsOne>
    
    CONVERT VM TO '~' IN Y.TT.CUS.CELL.DR

    EB.DataAccess.FRead(FN.AC,Y.DR.AC.ID,R.DR.AC.REC,F.AC,Y.DR.AC.ERR)

* Y.DR.AC.BR = R.DR.AC.REC<AC.CO.CODE>
    Y.DR.AC.BR = R.DR.AC.REC<AC.AccountOpening.Account.CoCode>

    LOCATE Y.DR.AC.BR IN AC.BR SETTING Y.DR.AC.BR.POS THEN
        Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SMS.OUT'
    END

    ELSE
        Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SSL.SMS.OUT/cbsfile'
    END

    !Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SSL.SMS.OUT/cbsfile'

    OPEN Y.SMS.BP TO F.SMS.OUT ELSE
        RETURN
    END

*  LOCATE R.DR.AC.REC<AC.CATEGORY> IN CAT.LIST SETTING Y.POS THEN
    LOCATE R.DR.AC.REC<AC.AccountOpening.Account.Category> IN CAT.LIST SETTING Y.POS THEN
* Y.DR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':R.NEW(TT.TE.NARRATIVE.2):'#':R.NEW(TT.TE.ACCOUNT.1):'#':'DR':'#':R.NEW(TT.TE.AMOUNT.LOCAL.1):'#':Y.TT.CUS.CELL.DR:'#':R.DR.AC.REC<AC.ONLINE.ACTUAL.BAL>:'#':Y.DR.CUS.ID:'#':R.DR.AC.REC<AC.CATEGORY>:'#':R.NEW(TT.TE.CO.CODE)
        Y.DR.SMS.OUT = Y.TT.ID:'#':Y.DT.TM:'#':Y.VAL:'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo):'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountOne):'#':'DR':'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeAmountLocalOne):'#':Y.TT.CUS.CELL.DR:'#':R.DR.AC.REC<AC.AccountOpening.Account.OnlineActualBal>:'#':Y.DR.CUS.ID:'#':R.DR.AC.REC<AC.AccountOpening.Account.Category>:'#':EB.SystemTables.getRNew(TT.Contract.Teller.TeCoCode)
        Y.DR.FILE.NAME = 'SMS_':Y.TT.ID:".":'DR':'.':Y.DATE.TIME

        WRITE Y.DR.SMS.OUT TO F.SMS.OUT,Y.DR.FILE.NAME

    END
RETURN
