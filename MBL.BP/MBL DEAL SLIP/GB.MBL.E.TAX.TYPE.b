* @ValidationCode : Mjo3MzA0ODE2OTc6Q3AxMjUyOjE2MzIxMzYyODY1Njk6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 20 Sep 2021 17:11:26
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.E.TAX.TYPE(Y.DATA)
    
*=============================================================================

*-----------------------------------------------------------------------------
* Modification History 1 :
* 20/09/2021 -                            Retrofit   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------
    

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*  $INSERT I_F.TAX
    $USING ST.ChargeConfig
    $USING EB.DataAccess
    
    FN.TAX = 'F.TAX' ;
    F.TAX = ''
    Y.DATE =''
    Y.T.ID =''
    Y.TX.ID =''
    Y.MAX.DATE =''

    EB.DataAccess.Opf(FN.TAX,F.TAX)
    Y.TAX.ID = Y.DATA
    
    SEL.CMD = "SELECT ":FN.TAX:" WITH @ID LIKE ":Y.TAX.ID :"..."
    
    EB.DataAccess.Readlist(SEL.CMD,SEL.LIST,'',NO.OF.RECORDS,RETURN.CODE)
    IF NO.OF.RECORDS > 1 THEN
        LOOP
            REMOVE Y.T.ID FROM SEL.LIST SETTING Y.POS
        WHILE Y.T.ID:Y.POS
            Y.TX.ID = Y.T.ID
            Y.DATE = RIGHT(Y.TX.ID,8)
            IF (Y.MAX.DATE) < Y.DATE THEN
                Y.MAX.DATE = Y.DATE
            END
        REPEAT

        Y.TT.ID =  Y.TAX.ID:".":Y.MAX.DATE
    END
    
    ELSE
        REMOVE Y.T.ID FROM SEL.LIST SETTING Y.POS
        Y.TX.ID = Y.T.ID
        Y.TT.ID = Y.TX.ID
    END
    EB.DataAccess.FRead(FN.TAX,Y.TT.ID,R.TAX,F.TAX,TAX.ERR)
* Y.TAX.AC = R.TAX<EB.TAX.SHORT.DESCR>
    Y.TAX.AC = R.TAX<ST.ChargeConfig.Tax.EbTaxShortDescr>

    Y.DATA = Y.TAX.AC

RETURN
END

