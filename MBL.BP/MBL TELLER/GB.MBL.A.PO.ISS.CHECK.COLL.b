* @ValidationCode : MjoxMDA0MjMzODAxOkNwMTI1MjoxNjI1NDA3MjkyMzA1OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 04 Jul 2021 20:01:32
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.A.PO.ISS.CHECK.COLL
*-----------------------------------------------------------------------------
* Description: This is an AUTH Routine attached to VERSION.CONTROL of
* FUNDS.TRANSFER. It retreives the PO Issue data for PO COLLECTION & CANCELLATION
*-----------------------------------------------------------------------------
* Modification History :
* 04/02/2021 - CREATED BY                  MD SHIBLI MOLLAH --FDS Bangladesh Limited
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING FT.Contract
    $USING TT.Contract
    $USING ST.ChqSubmit
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
    $USING EB.Updates
    $USING EB.TransactionControl
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
    FN.TELLER.NAU='F.TELLER$NAU'
    F.TELLER.NAU=''
    FN.FT='F.FUNDS.TRANSFER'
    F.FT=''
    FN.FT.NAU='F.FUNDS.TRANSFER$NAU'
    F.FT.NAU=''
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc> </desc>
    EB.DataAccess.Opf(FN.CHQ.REG.SUP, F.CHQ.REG.SUP)
    EB.DataAccess.Opf(FN.TELLER, F.TELLER)
    EB.DataAccess.Opf(FN.TELLER.NAU, F.TELLER.NAU)
    EB.DataAccess.Opf(FN.FT, F.FT)
    EB.DataAccess.Opf(FN.FT.NAU, F.FT.NAU)
RETURN
*** </region>


*--------------------------------------------------------------------------------------------*

*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>
    GOSUB GET.LOCAL.REF ; *
    
    Y.PO.COLL.REF=EB.SystemTables.getIdNew()
    Y.PO.COLL.CO=EB.SystemTables.getIdCompany()
    
    IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
*--------------------------TT ID----REV-------NAU---------------------------*
        Y.TT.ID = EB.SystemTables.getIdNew()
        EB.DataAccess.FRead(FN.TELLER, Y.TT.ID, REC.TT, F.TELLER, ERR.TT)
        Y.PO.AC = REC.TT<TT.Contract.Teller.TeAccountOne>
        Y.PO.NO = REC.TT<TT.Contract.Teller.TeStockNumber>
        Y.TT.REC.STATUS = REC.TT<TT.Contract.Teller.TeRecordStatus>
        Y.TT.TR.CODE = REC.TT<TT.Contract.Teller.TeTransactionCode>
    END

*------------------------FT------------------------------------------------------------*
    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN

*---------------FT NAU-------------IF RNAU THEN BOTH TELLER & FT FOR CRS WILL BE CLEARED--------*
        Y.FT.ID = EB.SystemTables.getIdNew()
        EB.DataAccess.FRead(FN.FT, Y.FT.ID, REC.FT, F.FT, ERR.FT)
        Y.PO.AC = REC.FT<FT.Contract.FundsTransfer.CreditAcctNo>
        Y.PO.NO = REC.FT<FT.Contract.FundsTransfer.StockNumber>
        Y.FT.REC.STATUS = REC.FT<FT.Contract.FundsTransfer.RecordStatus>
        Y.FT.TR.TYPE = REC.FT<FT.Contract.FundsTransfer.TransactionType>
    END
        
*----------------------------------------CRS------------------------------------------*
    Y.ID='PO.':Y.PO.AC:".":Y.PO.NO
    EB.DataAccess.FRead(FN.CHQ.REG.SUP, Y.ID, Rec.CHQ.REG.SUP, F.CHQ.REG.SUP, Y.Err)
    Y.CRS.STATUS = Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsStatus>
    Y.PO.CRS.DT = Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef, Y.CRS.COLL.DAT.POS>
    Y.PO.CRS.REF = Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef, Y.CRS.COLL.REF.POS>
    Y.PO.CRS.CO = Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef, Y.CRS.COLL.CO.POS>
*----ADD NEW PO CALCEL---
* Rec.CHQ.REG.SUP<ST.ChqSubmit.ChequeRegisterSupplement.CcCrsLocalRef,Y.CRS.PO.CANCL.POS>=Y.PO.CANCEL

***REQ --- CAN NOT BE REV IF COLLECTION ISN'T REV--------------***
***For allowing TT reverse if Transaction code 115, then Lt Crs Coll Dat field of CHEQUE.REGISTER.SUPPLEMENT should be null.
    IF Y.TT.TR.CODE EQ "115" AND Y.PO.CRS.DT NE "" THEN
        EB.SystemTables.setEtext("PO Already Collected!!!")
        EB.ErrorProcessing.StoreEndError()
    END
    
***For allowing FT reverse if Transaction type ACPA, then Lt Crs Coll Dat field of CHEQUE.REGISTER.SUPPLEMENT should be null.
    IF Y.FT.TR.TYPE EQ "ACPA" AND Y.PO.CRS.DT NE "" THEN
        EB.SystemTables.setEtext("PO Already Collected!!!")
        EB.ErrorProcessing.StoreEndError()
    END

RETURN
*** </region>


*--------------------ADD LT.CRS.PO.CANCL---------------------------------------------------------

*** <region name= GET.LOCAL.REF>
GET.LOCAL.REF:
*** <desc> </desc>
    Y.APP.NAME ="CHEQUE.REGISTER.SUPPLEMENT":FM:"TELLER":FM:"FUNDS.TRANSFER"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.CRS.COLL.DAT":VM:"LT.CRS.COLL.REF":VM:"LT.CRS.COLL.CO":VM:"LT.CRS.PO.CANCL":FM:"LT.TT.PO.CANCEL":FM:"LT.TT.PO.CANCEL"
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(Y.APP.NAME,LOCAL.FIELDS,FLD.POS)
    Y.CRS.COLL.DAT.POS=FLD.POS<1,1>
    Y.CRS.COLL.REF.POS=FLD.POS<1,2>
    Y.CRS.COLL.CO.POS=FLD.POS<1,3>
    Y.CRS.PO.CANCL.POS = FLD.POS<1,4>
    Y.TT.PO.CANCEL.POS = FLD.POS<2,1>
    Y.FT.PO.CANCEL.POS = FLD.POS<3,1>
    
RETURN
*** </region>

END