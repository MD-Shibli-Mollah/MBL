* @ValidationCode : MjoyMTAxNjI5Mjc2OkNwMTI1MjoxNjI4NjAwODA3MDQzOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 10 Aug 2021 19:06:47
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.V.GET.PO.COLL.TT.FT
*-----------------------------------------------------------------------------
*Subroutine Description:
* THIS ROUTINE USE FOR GET PO COLLECTION INFORMATION FORM CHEQUE.REGISTER.SUPPLEMENT
* BASE ON GIVEN ISSUED PO ORDER NO
*Attached To    : VERSION(FUNDS.TRANSFER,MBL.PO.COLLECTION  FUNDS.TRANSFER,MBL.IW.PO.COLLECTION TELLER,MBL.PO.PAY.CASH)
*Attached As    : VALIDATION ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 14/10/2020 - CREATED BY                         NEW - MD. SAROWAR MORTOZA
*                                                 FDS Bangladesh Limited
* 24/02/2021 - MODIFIED BY ---            	      MD SHIBLI MOLLAH --FDS Bangladesh Limited
* 18TH MARCH 2021 MODIFIED BY ---                 MD SHIBLI MOLLAH --FDS Bangladesh Limited
* UPDATE REQUIRED --TT -- LT.TT.ISS.DATE, LT.BRANCH, LT.TT.REF.NUM ; FT -- LT.FT.CONT.DATE, LT.ISSUE.BRANCH, LT.FT.REF.NO
* FROM CRS -- ISSUE.DATE, ORIGIN.REF, CO.CODE
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING FT.Contract
    $USING TT.Contract
    $USING ST.ChqSubmit
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Foundation
    $USING EB.Updates
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *
    GOSUB OPENFILE ; *
    GOSUB PROCESS ; *
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc> </desc>
    FN.CHQ.REG.SUP='F.CHEQUE.REGISTER.SUPPLEMENT'
    F.CHQ.REG.SUP=''
    FN.TELLER='F.TELLER'
    F.TELLER=''
    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    
    Y.APP.NAME ="CHEQUE.REGISTER.SUPPLEMENT"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.CRS.PUR.NAME"
    FLD.POS = ""
    EB.Foundation.MapLocalFields(Y.APP.NAME,LOCAL.FIELDS,FLD.POS)
    Y.CRS.PUR.NAME.POS=FLD.POS<1,1>
    FLD.POS = ""
*----------------------------------------------------------------

    APPLICATION.NAMES = 'TELLER':FM:'FUNDS.TRANSFER'
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = 'LT.TT.ISS.DATE':VM:'LT.BRANCH':VM:'LT.TT.REF.NUM':VM:'LT.PUR.NAME':FM:'LT.FT.CONT.DATE':VM:'LT.ISSUE.BRANCH':VM:'LT.FT.REF.NO':VM:'LT.CHQ.COM.CODE'
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(APPLICATION.NAMES, LOCAL.FIELDS, FLD.POS)
    Y.LT.TT.ISS.DATE.POS = FLD.POS<1,1>
    Y.TT.LT.BRANCH.POS = FLD.POS<1,2>
    Y.LT.TT.REF.NUM.POS = FLD.POS<1,3>
    Y.LT.PUR.NAME.POS = FLD.POS<1,4>
    Y.LT.FT.CONT.DAT.POS = FLD.POS<2,1>
    Y.LT.FT.ISSUE.BR.POS = FLD.POS<2,2>
    Y.LT.FT.REF.NO.POS = FLD.POS<2,3>
    Y.ISS.BR.CODE.POS = FLD.POS<2,4>
    
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc> </desc>
    EB.DataAccess.Opf(FN.CHQ.REG.SUP, F.CHQ.REG.SUP)
    EB.DataAccess.Opf(FN.TELLER, F.TELLER)
    EB.DataAccess.Opf(FN.FT, F.FT)
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>

    IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
        Y.ID.COMPANY = EB.SystemTables.getIdCompany()
        Y.PO.NO = EB.SystemTables.getComi()
        Y.PO.ID = 'PO.BDT152610001':Y.ID.COMPANY[6,4]:'.':Y.PO.NO
    END ELSE
        Y.ISS.BR.CODE.TEMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
        Y.ISS.BR.CODE = Y.ISS.BR.CODE.TEMP<1,Y.ISS.BR.CODE.POS>
    
        Y.PO.NO = EB.SystemTables.getComi()
   
        Y.PO.ID = 'PO.BDT152610001':Y.ISS.BR.CODE[6,4]:'.':Y.PO.NO
    END
    
    EB.DataAccess.FRead(FN.CHQ.REG.SUP, Y.PO.ID, Rec.PO, F.CHQ.REG.SUP, Y.Err)
    
    IF Rec.PO THEN
        Y.PO.STATUS=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsStatus>
        Y.PO.CURRENCY=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsCurrency>
        Y.PO.AMT=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsAmount>
        Y.PO.PAYEE.NAME=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsPayeeName>
        Y.PO.ISS.DATE=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsIssueDate>
        Y.PO.ORGIN=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsOrigin>
        Y.PO.ORGIN.REF=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsOriginRef>
        Y.PO.CHQ.TYP=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsIdCompOne>
*----PO FROM OTHER BRANCH--- ID COMP--ACC NUM---------
        Y.PO.AC.NO=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsIdCompTwo>
        Y.PUR.NAME=Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.PUR.NAME.POS>
*ADD ISSUE.DATE, ORIGIN.REF & CO.CODE --------------
        Y.ISSUE.DATE = Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsIssueDate>
        Y.ORIGIN.REF = Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsOriginRef>
        Y.CO.CODE = Rec.PO<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsCoCode>
    END ELSE
        EB.SystemTables.setEtext('Payorder Is Not Issued/Wrong Issue Branch')
        EB.ErrorProcessing.StoreEndError()
    END
           
*    IF EB.SystemTables.getApplication() EQ 'TELLER' AND Y.PO.ORGIN EQ 'TELLER' THEN
    IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
        EB.SystemTables.setRNew(TT.Contract.Teller.TePayeeName, Y.PO.PAYEE.NAME)
        EB.SystemTables.setRNew(TT.Contract.Teller.TeCurrencyOne, Y.PO.CURRENCY)
        EB.SystemTables.setRNew(TT.Contract.Teller.TeAmountLocalOne, Y.PO.AMT)
*----PO FROM OTHER BRANCH--- ID COMP--ACC NUM--Y.PO.AC.NO-------modified on 24th feb 2021 - SHIBLI FDS-
        EB.SystemTables.setRNew(TT.Contract.Teller.TeAccountTwo, Y.PO.AC.NO)
        
*-----PO ISSUE DATE -- TT -- LT.TT.ISS.DATE, LT.BRANCH, LT.TT.REF.NUM -------UPDATE for INVALID FUNC KEY-----
        Y.TEMP = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
        Y.TEMP<1,Y.LT.TT.ISS.DATE.POS> = Y.ISSUE.DATE
*-------LT TT BRANCH----------------------------------------
        Y.TEMP<1,Y.TT.LT.BRANCH.POS> = Y.CO.CODE
*-------TT FT REF NO-----------------------------------------
        Y.TEMP<1,Y.LT.TT.REF.NUM.POS> = Y.ORIGIN.REF
*-------PURCHASER NAME---------------------------------------
        Y.TEMP<1,Y.LT.PUR.NAME.POS> = Y.PUR.NAME
        EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef,Y.TEMP)
*--------------------UPDATE for INVALID FUNC KEY--END------------------------------------------
    END
    
*    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' AND Y.PO.ORGIN EQ 'FUNDS.TRANSFER' THEN
    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.DebitAcctNo, Y.PO.AC.NO)
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditCurrency, Y.PO.CURRENCY)
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.CreditAmount, Y.PO.AMT)
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.IssueChequeType, Y.PO.CHQ.TYP)
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.PayeeName, Y.PO.PAYEE.NAME)
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.PaymentDetails, Y.PUR.NAME)
        
*----------LT.FT.CONT.DATE, LT.ISSUE.BRANCH, LT.FT.REF.NO----------------------UPDATE for INVALID FUNC KEY----------------
        Y.FT.TEMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
*-----------LT.ISSUE.DATE----------------------------------------------------------
        
        Y.FT.TEMP<1,Y.LT.FT.CONT.DAT.POS> = Y.ISSUE.DATE
*-----------LT.ISSUE.BR----------------------------------------------------------
    
        Y.FT.TEMP<1,Y.LT.FT.ISSUE.BR.POS> = Y.CO.CODE
*-----------LT.ISSUE.ORIGIN.REF----------------------------------------------------------
  
        Y.FT.TEMP<1,Y.LT.FT.REF.NO.POS> = Y.ORIGIN.REF
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.LocalRef, Y.FT.TEMP)
*-------------------------UPDATE for INVALID FUNC KEY-----END------------------------------------------
    END

RETURN
*** </region>

END