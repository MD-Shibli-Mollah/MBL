* @ValidationCode : MjotMTQ4MzAwODQ4MDpDcDEyNTI6MTYyNDQ2NTA5OTk1Nzp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 23 Jun 2021 22:18:19
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.V.TIL.TRF.VAL
*-----------------------------------------------------------------------------

***********************************************************************
*
* Company Name   : FDS BD Ltd
* Developed By   : MD SHIBLI MOLLAH
* Modified By    :
*----------------------------------------------------------------------
*Subroutine Type:
*
*Attached To    : Teller Version (TELLER,MBL.LCY.TILLTFR, TELLER,MBL.FCY.TILLTFR)
*
*Attached As    : VALIDATION ROUTINE FOR THE FIELD "TELLER.ID.1" AND TELLER.ID.2
*
*------------------------------------------
*Description
*------------
* This Routine will restric the Vault Id Transaction in "Lcy Till Transfer" and "Fcy Till Transfer"
* This Routine will restric the other branch's teller id and Head-Teller id.
*------------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $USING TT.Contract
    $USING EB.DataAccess
    $USING EB.SystemTables
    $USING EB.ErrorProcessing
*------------------------------------------------------------------------------------

    GOSUB INIT
    GOSUB PROCESS

RETURN
*------------------------------------------------------------------------------------
INIT:
    FN.TT.ID = 'F.TELLER.ID'
    F.TT.ID = ''
    EB.DataAccess.Opf(FN.TT.ID,F.TT.ID)

RETURN
*------------------------------------------------------------------------------------
PROCESS:

*This is for restrict the Vault Id
    Y.ID = EB.SystemTables.getComi();
    IF Y.ID EQ 9999 THEN
*        ETEXT = "EB-TILL.VAULT"
*        CALL STORE.END.ERROR
        EB.SystemTables.setEtext('Vault Teller ID can not be used')
        EB.ErrorProcessing.StoreEndError()
    END

*This is for restrict the other branch's Teller id
    Y.COMPANY = EB.SystemTables.getIdCompany()
    EB.DataAccess.FRead(FN.TT.ID,Y.ID,R.TT.ID,F.TT.ID,Y.ERR)
* Y.COMP = R.TT.ID<TT.TID.CO.CODE>
    Y.COMP = R.TT.ID<TT.Contract.TellerId.TidCoCode>
    IF Y.COMPANY NE Y.COMP THEN
        EB.SystemTables.setEtext('Teller id is not in this Branch')
        EB.ErrorProcessing.StoreEndError()
    END
RETURN

*------------------------------------------------------------------------------------
END
