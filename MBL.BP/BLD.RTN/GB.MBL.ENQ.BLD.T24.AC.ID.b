* @ValidationCode : MjotMTgxOTAyNjMyMTpDcDEyNTI6MTYzMzIwMzc0OTg3Njp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 03 Oct 2021 01:42:29
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

* @AUTHOR         : MD SHIBLI MOLLAH

* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 16TH JULY 2021
* CONSIDERED FOR TT AS WELL           on 23RD SEP 2021

* Attached to MBL.IMAGE.VIEW.SIGN
*---------------------------------------------------

SUBROUTINE GB.MBL.ENQ.BLD.T24.AC.ID(ENQ.DATA)
    
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    
    
    $USING FT.Contract
    $USING TT.Contract
    $USING EB.DataAccess
    $USING EB.Reports
    $USING AC.AccountOpening
*------------------------------------------
 
    FN.ALT.AC = 'F.ALTERNATE.ACCOUNT'
    F.ALT.AC = ''
    
    FN.FT = "F.FUNDS.TRANSFER$NAU"
    F.FT = ""
    
    FN.TT = "F.TELLER$NAU"
    F.TT = ""
     
    EB.DataAccess.Opf(FN.ALT.AC, F.ALT.AC)

    Y.FIELD.NAME = 'IMAGE.REFERENCE'
    Y.ENQ.SEL = EB.Reports.getEnqSelection()
        
    LOCATE Y.FIELD.NAME IN Y.ENQ.SEL<2,1> SETTING ACCOUNT.NO.POS THEN
        Y.ACC.NO = Y.ENQ.SEL<4,ACCOUNT.NO.POS>
        
******************IMAGE FOR FT*****************************
        Y.ID.LEN = Y.ACC.NO[1,2]
        IF Y.ID.LEN EQ 'FT' THEN
            Y.FT.NO = Y.ACC.NO
    
            EB.DataAccess.Opf(FN.FT, F.FT)
            EB.DataAccess.FRead(FN.FT, Y.FT.NO, REC.FT, F.FT, ERR.FT)
            Y.ACC.NO = REC.FT<FT.Contract.FundsTransfer.DebitAcctNo>
        END
  
*******************IMAGE FOR TT*************************************
        Y.ID.LEN = Y.ACC.NO[1,2]
        IF Y.ID.LEN EQ 'TT' THEN
            Y.TT.NO = Y.ACC.NO
    
            EB.DataAccess.Opf(FN.TT, F.TT)
            EB.DataAccess.FRead(FN.TT, Y.TT.NO, REC.TT, F.TT, ERR.TT)
            Y.ACC.NO = REC.TT<TT.Contract.Teller.TeAccountTwo>
        END

********************************************************************
        IF Y.ACC.NO NE '' THEN
            EB.DataAccess.FRead(FN.ALT.AC, Y.ACC.NO, R.ALT.AC, F.ALT.AC, ERR.ALT.AC)
            Y.AC = R.ALT.AC<AC.AccountOpening.AlternateAccount.AacGlobusAcctNumber>
            
            IF Y.AC EQ '' THEN
                Y.AC = Y.ACC.NO
            END
        END
                
        ENQ.DATA<4,ACCOUNT.NO.POS> = Y.AC

        RETURN
    END