* @ValidationCode : MjotNzE2NDk3MjgzOkNwMTI1MjoxNjI0MzU3NzMwNzcyOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 22 Jun 2021 16:28:50
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.I.TT.FT.CARD.NUMBER.AC
*-----------------------------------------------------------------------------
* Subroutine Description:
* This routine is used TO VALIDATE CARD HOLDER NAME
* Subroutine Type:
* Attached To    : Version (TELLER,MBL.CARD.LOCAL TELLER,MBL.CARD.INT FUNDS.TRANSFER,MBL.CARD.LOCAL AND FUNDS.TRANSFER,MBL.CARD.INT)
* Attached As    : INPUT ROUTINE
*-----------------------------------------------------------------------------
* Modification History :
* 15/07/2020 -                            Retrofit   - Sarowar Mortoza
*                                                     FDS Pvt Ltd
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING TT.Contract
    $USING FT.Contract
    $USING ST.CurrencyConfig
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.Updates
    $USING EB.ErrorProcessing
    $INSERT I_F.MBL.CREDIT.CARD.DETAILS
*-----------------------------------------------------------------------------
    GOSUB INITIALISE ; *
    GOSUB OPENFILE ; *
    GOSUB PROCESS ; *
RETURN
*-----------------------------------------------------------------------------

*** <region name= INITIALISE>
INITIALISE:
*** <desc> </desc>
    Y.CARD.NO = ''
    Y.CARD.TYPE = ''
    Y.CUST.NAME = ''
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= OPENFILE>
OPENFILE:
*** <desc> </desc>
    FN.CARD = 'F.MBL.CREDIT.CARD.DETAILS'
    F.CARD = ''
    EB.DataAccess.Opf(FN.CARD,F.CARD)

    FN.TT = 'F.TELLER'
    F.TT = ''
    EB.DataAccess.Opf(FN.TT,F.TT)
    
    FN.FT = 'F.FUNDS.TRANSFER'
    F.FT = ''
    EB.DataAccess.Opf(FN.FT,F.FT)
    
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>
    Y.APP.NAME ="TELLER":FM:"FUNDS.TRANSFER"
    LOCAL.FIELDS = ""
    LOCAL.FIELDS = "LT.TT.NAME":FM:"LT.FT.CR.NARR":VM:"LT.TT.NAME"
    FLD.POS = ""
    EB.Updates.MultiGetLocRef(Y.APP.NAME, LOCAL.FIELDS,FLD.POS)
    Y.TT.NAME.POS=FLD.POS<1,1>
    Y.FT.CARDNO.POS=FLD.POS<2,1>
    Y.FT.NAME.POS=FLD.POS<2,2>

    IF EB.SystemTables.getApplication() EQ 'TELLER' THEN
        Y.CARD.NO=EB.SystemTables.getRNew(TT.Contract.Teller.TeNarrativeTwo)
        EB.DataAccess.FRead(FN.CARD,Y.CARD.NO,R.CARD,F.CARD,CARD.ERR)
        GOSUB PROCESS.TT.TXN ; *
    END
    
    IF EB.SystemTables.getApplication() EQ 'FUNDS.TRANSFER' THEN
        Y.CARD.NO=EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)<1,Y.FT.CARDNO.POS>
        EB.DataAccess.FRead(FN.CARD,Y.CARD.NO,R.CARD,F.CARD,CARD.ERR)
        GOSUB PROCESS.FT.TXN ; *
    END
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS.TT.TXN>
PROCESS.TT.TXN:
*** <desc> </desc>
    IF R.CARD THEN
        Y.CUST.NAME = R.CARD<CR.CARD.CUST.NAME>
        
        Y.TEMP = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
        Y.TEMP<1,Y.TT.NAME.POS> = Y.CUST.NAME
        EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef, Y.TEMP)
    END ELSE
        Y.TEMP = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
        Y.TEMP<1,Y.TT.NAME.POS> = ''
        EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef, Y.TEMP)
        
        EB.SystemTables.setRNew(TT.Contract.Teller.TeNarrativeTwo, Y.CARD.NO)
        EB.SystemTables.setEtext("Invalid Card No")
        EB.ErrorProcessing.StoreEndError()
    END
 
    IF EB.SystemTables.getPgmVersion() EQ ",MBL.CARD.INT" THEN
        Y.FCY.ACC =R.CARD<CR.CARD.FCY>
        IF Y.FCY.ACC EQ '' THEN
            EB.SystemTables.setEtext("NOT AN INTERNATIONAL CARD")
            EB.ErrorProcessing.StoreEndError()
        END
    END
    
    IF EB.SystemTables.getPgmVersion() EQ ",MBL.CARD.LOCAL" THEN
        Y.LCY.ACC =R.CARD<CR.CARD.LCY>
        IF Y.LCY.ACC EQ '' THEN
            EB.SystemTables.setEtext("NOT A LOCAL CARD")
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
*** </region>


*-----------------------------------------------------------------------------

*** <region name= PROCESS.FT.TXN>
PROCESS.FT.TXN:
*** <desc> </desc>
    IF R.CARD THEN
        Y.CUST.NAME = R.CARD<CR.CARD.CUST.NAME>
         
        Y.TEMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
        Y.TEMP<1,Y.FT.NAME.POS> = Y.CUST.NAME
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.LocalRef, Y.TEMP)
    END ELSE
        Y.TEMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
        Y.TEMP<1,Y.FT.NAME.POS> = ''
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.LocalRef, Y.TEMP)

        Y.TEMP = EB.SystemTables.getRNew(FT.Contract.FundsTransfer.LocalRef)
        Y.TEMP<1,Y.FT.CARDNO.POS> = Y.CARD.NO
        EB.SystemTables.setRNew(FT.Contract.FundsTransfer.LocalRef, Y.TEMP)
        
        EB.SystemTables.setEtext("Invalid Card No")
        EB.ErrorProcessing.StoreEndError()
    END
    
    IF EB.SystemTables.getPgmVersion() EQ ",MBL.CARD.INT" THEN
        Y.FCY.ACC =R.CARD<CR.CARD.FCY>
        
        IF Y.FCY.ACC EQ '' THEN
            EB.SystemTables.setEtext("NOT AN INTERNATIONAL CARD")
            EB.ErrorProcessing.StoreEndError()
        END
    END
    
    IF EB.SystemTables.getPgmVersion() EQ ",MBL.CARD.LOCAL" THEN
        Y.LCY.ACC =R.CARD<CR.CARD.LCY>
        
        IF Y.LCY.ACC EQ '' THEN
            EB.SystemTables.setEtext("NOT A LOCAL CARD")
            EB.ErrorProcessing.StoreEndError()
        END
    END
RETURN
*** </region>

END





