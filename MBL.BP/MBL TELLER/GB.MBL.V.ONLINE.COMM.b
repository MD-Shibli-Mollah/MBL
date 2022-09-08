* @ValidationCode : MjotMTkwNDYwNTI5MjpDcDEyNTI6MTYyNzczODc0NDQyOTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 31 Jul 2021 19:39:04
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.V.ONLINE.COMM
*-----------------------------------------------------------------------------
* Modification History : 1    Retrofit by: Mahmudur Rahman Udoy  Date:6/24/2021
* OLD Routine: E.ONLINE.COMM
*-----------------------------------------------------------------------------
* Modification History : 2    MOdified for Input RTN due to multiple Val Msg
*-----------------------------------------------------------------------------
* CASE FUNCTION FIXED
* Y.VER FOR VERSION ADDED.
* Modified by ---- MD SHIBLI MOLLAH FDS -- on 29TH JUL 2021
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE

    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING AC.AccountOpening
    $USING TT.Contract

*    FN.TELLER = 'F.TELLER'
*    F.TELLER = ''
 
    FN.ACCOUNT='F.ACCOUNT'
    F.ACCOUNT=''
    
*  EB.DataAccess.Opf(FN.TELLER,F.TELLER)
    EB.DataAccess.Opf(FN.ACCOUNT,F.ACCOUNT)

* Y.ACCOUNT= EB.SystemTables.getComi()
    Y.VER = EB.SystemTables.getPgmVersion()
    Y.ACCOUNT= EB.SystemTables.getRNew(TT.Contract.Teller.TeAccountTwo)
    Y.COMPANY= EB.SystemTables.getIdCompany()
    
    EB.DataAccess.FRead(FN.ACCOUNT,Y.ACCOUNT,AC.REC,F.ACCOUNT,AC.ERR)
    Y.CO.CODE=AC.REC<AC.AccountOpening.Account.CoCode>
*    Y.ONLINE.CHG=AC.REC<AC.LOCAL.REF,124>
*    Y.ONLINE.RES=AC.REC<AC.LOCAL.REF,125>


    BEGIN CASE
        CASE Y.VER EQ ',MBL.LCY.CASHIN'
            IF Y.COMPANY = Y.CO.CODE THEN
                EB.SystemTables.setRNew(TT.Contract.Teller.TeTransactionCode,'10')
            END ELSE
                EB.SystemTables.setRNew(TT.Contract.Teller.TeTransactionCode,'35')
            END

        CASE Y.VER EQ ',MBL.LCY.CASHCHQ'
            IF Y.COMPANY = Y.CO.CODE THEN
                EB.SystemTables.setRNew(TT.Contract.Teller.TeTransactionCode,'14')
            END ELSE
                EB.SystemTables.setRNew(TT.Contract.Teller.TeTransactionCode,'36')
            END

    END CASE

*------------------------TRACER------------------------------------------------------
*    WriteData = "VERSION NAME: ":Y.VER:" ":Y.CO.CODE:" ":Y.ACCOUNT
*    FileName = 'SH_ONLINE_21.txt'
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
**

**********************************************

RETURN
END

