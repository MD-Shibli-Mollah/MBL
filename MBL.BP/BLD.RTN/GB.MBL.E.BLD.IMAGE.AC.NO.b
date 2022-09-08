* @ValidationCode : MjotMTgzNTUyMDEwMTpDcDEyNTI6MTYzMzIwMjIxNzEzNjp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 03 Oct 2021 01:16:57
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.E.BLD.IMAGE.AC.NO(ENQ.DATA)
    
* @AUTHOR         : MD SHIBLI MOLLAH

* Modification History :
*-----------------------------------------------------------------------------
* Modified by MD SHIBLI MOLLAH FDS -- on 16TH JULY 2021
* CONSIDERED FOR TT AS WELL           on 23RD SEP 2021
* Attached to - MBL.ENQ.AUTH.SIGN.VIEW
* Final Modification                  on 03rd OCT 2021
*----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    
    $USING FT.Contract
    $USING TT.Contract
    $USING EB.DataAccess
    $USING AC.AccountOpening
        
    
    FN.TT = 'F.TELLER$NAU'
    F.TT = ''
    EB.DataAccess.Opf(FN.TT, F.TT)
    
    FN.FT = 'F.FUNDS.TRANSFER$NAU'
    F.FT = ''
    EB.DataAccess.Opf(FN.FT, F.FT)
    
*---VERSION NAME is in (4,1) & TT OR FT ID is in (4,2)-----------------
    Y.DATA = ENQ.DATA<4,2>

        
    IF Y.DATA[1,2] EQ 'TT' THEN
        EB.DataAccess.FRead(FN.TT, Y.DATA, REC.TT, F.TT, ERR.TT)
        Y.ACC.NO = REC.TT<TT.Contract.Teller.TeAccountTwo>
    END
    
    IF Y.DATA[1,2] EQ 'FT' THEN
        EB.DataAccess.FRead(FN.FT, Y.DATA, REC.FT, F.FT, ERR.FT)
        Y.ACC.NO = REC.FT<FT.Contract.FundsTransfer.DebitAcctNo>
    END
    
    ENQ.DATA<4,1> = Y.ACC.NO
    ENQ.DATA<4,2> = Y.ACC.NO
 
*    WriteData = ENQ.DATA:' ':Y.DATA.POS:" ":Y.ACC.NO:" Y.DATA: ":Y.DATA
*    FileName = 'SHIBLI_img.BLD.txt'
*    FilePath = 'DL.BP'
*    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
*    ELSE
*        CREATE FileOutput ELSE
*        END
*    END
*    WRITESEQ WriteData APPEND TO FileOutput ELSE
*        CLOSESEQ FileOutput
*    END
*    CLOSESEQ FileOutput
 
RETURN
END