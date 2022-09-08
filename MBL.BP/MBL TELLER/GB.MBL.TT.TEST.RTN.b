* @ValidationCode : MjoxMzQ0ODY5OTM6Q3AxMjUyOjE2MjAyNzgzMjgzMTY6dXNlcjotMTotMTowOjA6ZmFsc2U6Ti9BOkRFVl8yMDE3MTAuMDotMTotMQ==
* @ValidationInfo : Timestamp         : 06 May 2021 11:18:48
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.TT.TEST.RTN

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
    $INSERT I_EB.TRANS.COMMON
    $INSERT I_ENQUIRY.COMMON
    $USING EB.SystemTables
    $USING EB.Updates
    $USING TT.Contract
    $USING EB.LocalReferences
    $USING EB.DataAccess
    $USING AC.AccountOpening
    
    
    Y.ACC = EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
        
    FN.ACC = "F.ACCOUNT"
    F.ACC = ""
    Y.TT.LT.BRANCH = ""
    
    EB.DataAccess.Opf(FN.ACC, F.ACC)
    
    EB.DataAccess.FRead(FN.ACC, Y.ACC, R.ACC, F.ACC, ACC.ERR)
    Y.CO.CODE = R.ACC<AC.AccountOpening.Account.CoCode>

    APPLICATION.NAME = 'TELLER'
    Y.FILED.NAME = 'LT.BRANCH'
    Y.FIELD.POS = ''
    EB.LocalReferences.GetLocRef(APPLICATION.NAME,Y.FILED.NAME,Y.FIELD.POS)
    Y.TT.LT.BRANCH = EB.SystemTables.getRNew(TT.Contract.Teller.TeLocalRef)
    Y.TT.LT.BRANCH<1,Y.FIELD.POS> = Y.CO.CODE
    EB.SystemTables.setRNew(TT.Contract.Teller.TeLocalRef,Y.TT.LT.BRANCH)
    
*--------------------------WRITE DATA---------------------------------
    WriteData = " ACC: ":Y.ACC:" CO CODE: ":Y.CO.CODE:" ":Y.FILED.NAME:" ":Y.FIELD.POS:" ":Y.TT.LT.BRANCH
    FileName = 'SHIBLI_TT.TEST.txt'
    FilePath = 'DL.BP'
    OPENSEQ FilePath,FileName TO FileOutput THEN NULL
    ELSE
        CREATE FileOutput ELSE
        END
    END
    WRITESEQ WriteData APPEND TO FileOutput ELSE
        CLOSESEQ FileOutput
    END
    CLOSESEQ FileOutput
*---------------------END--------------------------------------------
RETURN
END
