SUBROUTINE ISL.EB.FT.SMS.OUT
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $USING AC.AccountOpening
    $USING FT.Contract
    $USING ST.CompanyCreation
    $USING EB.SystemTables
    $USING EB.DataAccess
    $USING ST.Customer
*-----------------------------------------------------------------------------
* Modification History :
* 1)
*    Date : 20200712
*    Modification Description : retrofit
*    Modified By  :S.M. Sayeed
*    Designation  : Technical Consultant
*    Email        : s.m.sayeed@fortress-global.com
*-----------------------------------------------------------------------------

*************MODIFIED ON 16-08-2016 TO ALLOW SMS FOR PAYORDER***************

    Y.VERSION.NAME = EB.SystemTables.getPgmVersion()
    IF (Y.VERSION.NAME EQ ",PR.PO.ISSUE") OR (Y.VERSION.NAME EQ ",PR.PO.COLLECTION") THEN
        Y.CHK.AMOUNT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAmount)
    END
    ELSE

        Y.CHK.AMOUNT = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAmount)

    END

*****************************************************************

    !IF V$FUNCTION NE "A" OR Y.CHK.AMOUNT LT 5000 THEN RETURN

    IF EB.SystemTables.getVFunction() NE "A" THEN RETURN

* Please add all category in CAT.LIST which will propagate through the entire subroutine.
    
    CAT.LIST=CAT.LIST:@FM:'1201':@FM:'1202':@FM:'6201':@FM:'6200':@FM:'6290':@FM:'6230':@FM:'6300':@FM:'6225'
    CAT.LIST=CAT.LIST:@FM:'1922':@FM:'1924':@FM:'1935':@FM:'1925':@FM:'1933'
    


    !AC.BR ='BD0010111':@FM:'BD0010120':@FM:'BD0010148':@FM:'BD0010314':@FM:'BD0010101':@FM:'BD0010113':@FM:'BD0010115':@FM:'BD0010140':@FM:'BD0010121'

    !AC.BR ='BD0010001':@FM:'BD0010002':@FM:'BD0010101':@FM:'BD0010102':@FM:'BD0010103':@FM:'BD0010104':@FM:'BD0010105':@FM:'BD0010106':@FM:'BD0010107':@FM:'BD0010108':@FM:'BD0010109':@FM:'BD0010110':@FM:'BD0010111':@FM:'BD0010112':@FM:'BD0010113':@FM:'BD0010114':@FM:'BD0010115':@FM:'BD0010116':@FM:'BD0010117':@FM:'BD0010118':@FM:'BD0010119':@FM:'BD0010120':@FM:'BD0010121':@FM:'BD0010122':@FM:'BD0010123':@FM:'BD0010124':@FM:'BD0010125':@FM:'BD0010126':@FM:'BD0010127':@FM:'BD0010128':@FM:'BD0010129':@FM:'BD0010130':@FM:'BD0010131':@FM:'BD0010132':@FM:'BD0010133':@FM:'BD0010134':@FM:'BD0010135':@FM:'BD0010136':@FM:'BD0010137':@FM:'BD0010138':@FM:'BD0010139':@FM:'BD0010140':@FM:'BD0010141':@FM:'BD0010142':@FM:'BD0010143':@FM:'BD0010144':@FM:'BD0010145':@FM:'BD0010146':@FM:'BD0010147':@FM:'BD0010148':@FM:'BD0010149':@FM:'BD0010150':@FM:'BD0010154':@FM:'BD0010155':@FM:'BD0010156':@FM:'BD0010157':@FM:'BD0010158':@FM:'BD0010159':@FM:'BD0010160':@FM:'BD0010303'


    TXN.TYPE='ACNW':@FM:'AC':@FM:'ACCD':@FM:'ACCW':@FM:'AC01':@FM:'AC02':@FM:'ACP1':@FM:'ACPA':@FM:'ACRT':@FM:'ACAL':@FM:'ACEF':@FM:'ACIW':@FM:'ACDG'

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


    Y.FT.ID = EB.SystemTables.getIdNew()
    R.FT.REC = ''
    Y.FT.ERR = ''

    Y.TODAY =EB.SystemTables.getToday()
    TIME.ST = TIMEDATE()
    Y.DATE.TIME =Y.TODAY:'_':TIME.ST[1,2]:TIME.ST[4,2]:TIME.ST[7,2]
    Y.TM = TIME.ST[1,2]:':':TIME.ST[4,2]



    LOCATE EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TransactionType) IN TXN.TYPE SETTING Y.TXN.POS THEN



*******************
        !FT.CRAC.FILE.GEN
*******************

        Y.CR.AC.ID = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo)
        EB.DataAccess.FRead(FN.AC,Y.CR.AC.ID,R.CR.AC.REC,F.AC,Y.CR.AC.ERR)
        Y.CR.AC.BR = R.CR.AC.REC<AC.AccountOpening.Account.CoCode>

*        LOCATE Y.CR.AC.BR IN AC.BR SETTING Y.CR.AC.BR.POS THEN
*            Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SSL.SMS.OUT/cbsfile'
*        END
*        ELSE
*            Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SMS.OUT'
*        END
        Y.SMS.BP = '/t24/mbllive/SMS.ISL.IN'
        OPEN Y.SMS.BP TO F.SMS.OUT ELSE
            RETURN
        END

        Y.FT.CUS.ID.CR=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditCustomer)

        EB.DataAccess.FRead(FN.CUS,Y.FT.CUS.ID.CR,R.CUS.CR.REC,F.CUS,Y.CUS.ERR)
        Y.FT.CUS.CELL.CR=R.CUS.CR.REC<ST.Customer.Customer.EbCusSmsOne>

        LOCATE R.CR.AC.REC<AC.AccountOpening.Account.Category> IN CAT.LIST SETTING Y.POS THEN

            Y.CR.SMS.OUT = Y.FT.ID:'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.AuthDate):' ':Y.TM:'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TransactionType):'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditTheirRef):'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CreditAcctNo):'#':'CR':'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.AmountCredited):'#':Y.FT.CUS.CELL.CR:'#':R.CR.AC.REC<AC.AccountOpening.Account.OnlineActualBal>:'#':Y.FT.CUS.ID.CR:'#':R.CR.AC.REC<AC.AccountOpening.Account.Category>:'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CoCode)
            Y.CR.FILE.NAME = 'SMS_':Y.FT.ID:'.':Y.CR.AC.ID:".":'CR':'.':Y.DATE.TIME

            WRITE Y.CR.SMS.OUT TO F.SMS.OUT,Y.CR.FILE.NAME
        END

*******************
        !FT.DRAC.FILE.GEN:
*******************

        Y.DR.AC.ID = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo)
        EB.DataAccess.FRead(FN.AC,Y.DR.AC.ID,R.DR.AC.REC,F.AC,Y.DR.AC.ERR)
        Y.DR.AC.BR = R.DR.AC.REC<AC.AccountOpening.Account.CoCode>

*        LOCATE Y.DR.AC.BR IN AC.BR SETTING Y.DR.AC.BR.POS THEN
*            Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SSL.SMS.OUT/cbsfile'
*        END
*        ELSE
*            Y.SMS.BP = '/mblapp01fs/t24appprod/bnk/bnk.run/tcupload/SMS.OUT'
*        END
        Y.SMS.BP = '/t24/mbllive/SMS.ISL.IN'
        OPEN Y.SMS.BP TO F.SMS.OUT ELSE
            RETURN
        END

        Y.FT.CUS.ID.DR=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitCustomer)

        EB.DataAccess.FRead(FN.CUS,Y.FT.CUS.ID.DR,R.CUS.DR.REC,F.CUS,Y.CUS.ERR)
        Y.FT.CUS.CELL.DR=R.CUS.DR.REC<ST.Customer.Customer.EbCusSmsOne>

        LOCATE R.DR.AC.REC<AC.AccountOpening.Account.Category> IN CAT.LIST SETTING Y.POS THEN

            Y.DR.SMS.OUT = Y.FT.ID:'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.AuthDate):' ':Y.TM:'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.TransactionType):'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitTheirRef):'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.DebitAcctNo):'#':'DR':'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.AmountDebited):'#':Y.FT.CUS.CELL.DR:'#':R.DR.AC.REC<AC.AccountOpening.Account.OnlineActualBal>:'#':Y.FT.CUS.ID.DR:'#':R.DR.AC.REC<AC.AccountOpening.Account.Category>:'#':EB.SystemTables.getRNew(FT.Contract.FundsTransfer.CoCode)

            Y.DR.FILE.NAME ='SMS_':Y.FT.ID:'.': Y.DR.AC.ID:".":'DR.':Y.DATE.TIME
            WRITE Y.DR.SMS.OUT TO F.SMS.OUT,Y.DR.FILE.NAME
        END

        RETURN

    END
