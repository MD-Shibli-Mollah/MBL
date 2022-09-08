* @ValidationCode : MjozNzQxMDczNDc6Q3AxMjUyOjE2MzIxMzU0MzMwMTk6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 20 Sep 2021 16:57:13
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.E.COM.TYPE(Y.DATA)
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
*    $INSERT I_F.FT.COMMISSION.TYPE
*    $INSERT T24.BP I_F.CATEGORY
    $USING ST.ChargeConfig
    $USING ST.Config
    $USING EB.DataAccess
 
*=============================================================================
* Subroutine type    : Subroutine
* Attached to        : MBL.DS.FT.LCY
* Primary Purpose    : This Routine used to Converting Amount in words.
* Created by         : Md Rayhan Uddin
* Change History     : Manjunath Suvarna
*                    : Sudheep V
*=============================================================================

*-----------------------------------------------------------------------------
* Modification History 1 :
* 20/09/2021 -                            Retrofit   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------
 
    
INIT:
    FN.COM = 'F.FT.COMMISSION.TYPE'
    F.COM = ''
    
    FN.CAT='F.CATEGORY'
    F.CAT=''
    
    Y.CAT.ID=''
    Y.CAT.NAM=''
    EB.DataAccess.Opf(FN.COM,F.COM)
    EB.DataAccess.Opf(FN.CAT,F.COM)

PROCESS:
    Y.COM.ID = Y.DATA
    EB.DataAccess.FRead(FN.COM,Y.COM.ID,R.COM,F.COM,ACC.ERR)
* Y.COM.AC = R.COM<FT4.CATEGORY.ACCOUNT>
    Y.COM.AC = R.COM<ST.ChargeConfig.FtCommissionType.FtFouCategoryAccount>
   
    IF LEN(Y.COM.AC) > 5 THEN
        Y.CAT.ID =Y.COM.AC[4,5]
    END

    ELSE
        Y.CAT.ID =Y.COM.AC
    END

    EB.DataAccess.FRead(FN.CAT,Y.CAT.ID,R.CAT,F.CAT,ACC.ERR)
* Y.CAT.NAM = R.CAT<EB.CAT.DESCRIPTION>
    Y.CAT.NAM = R.CAT<ST.Config.Category.EbCatDescription>
    
    IF LEN(Y.COM.AC) = 5 THEN
        Y.DATA = 'PL-':Y.COM.AC:'  ':Y.CAT.NAM
    END
      
    ELSE
        Y.DATA = Y.COM.AC[1,8]:'  ':Y.CAT.NAM
    END


RETURN
END
