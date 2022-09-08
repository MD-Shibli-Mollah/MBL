* @ValidationCode : MjotMTg5MjA1OTUxOkNwMTI1MjoxNjMxMDk4Mjc1NzE4OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 08 Sep 2021 16:51:15
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE MBL.DFE.SET.CO.CODE(COM.CD.VAL)
    
*-----------------------------------------------------------------------------
* Description   : This is the post routine attached to DFE.PARAMETER
* Type          : DFE.PARAMETER>COMPANY.CODE
* DFE.PARAMETER   : MBL.DFE.FT.EFT.IW
* Linked With   : DFE.MAPPING
* In Parameter  :
* Out Parameter :
*-----------------------------------------------------------------------------
* Revision History:
*------------------
* Date         - 07/09/2021
* Done By      - Rayhan Uddin Sarkar
*-----------------------------------------------------------------------------

    $INSERT I_F.ACCOUNT
    $INSERT I_EQUATE
    $INSERT I_COMMON
    $USING EB.Utility
    $USING EB.DataAccess
	
    FN.AC = 'F.ACCOUNT'
    FV.AC = ''
    
*
    
    CALL OPF(FN.AC,FV.AC)
*  EB.DataAccess.Opf(FN.AC,F.AC)

    R.AC.REC = ''
    AC.EER = ''
    CHECK.REC = ''
    R.MAPPING.VALUE = ''
    APPL.FIELD.NAME = ''
    APPL.FIELD.POSN = ''
    Y.COMPANY.CODE = ''
    Y.CR.POS = ''
    Y.DR.POS = ''
    Y.CR.AC = ''
    Y.DR.AC = ''
    Y.AC.COM = ''
    
*
    
    CHECK.REC = EB.Utility.getCCurrentRecord()
    R.MAPPING.VALUE = EB.Utility.getCRMapping()
    APPL.FIELD.NAME = EB.Utility.getCRMapping()<EB.Utility.DfeMapping.DfeMapApplFieldName>
    APPL.FIELD.POSN = EB.Utility.getCRMapping()<EB.Utility.DfeMapping.DfeMapApplFieldPosn>
    
    LOCATE 'CREDIT.ACCT.NO' IN APPL.FIELD.NAME<1,1> SETTING Y.CR.POS THEN
        Y.CR.AC = FIELD(CHECK.REC,',',Y.CR.POS)
    END
    LOCATE 'DEBIT.ACCT.NO' IN APPL.FIELD.NAME<1,1> SETTING Y.DR.POS THEN
        Y.DR.AC = FIELD(CHECK.REC,',',Y.DR.POS)
    END
    IF Y.CR.AC[1,3] EQ 'BDT' THEN
        CALL F.READ(FN.AC,Y.CR.AC,R.AC.REC,FV.AC,AC.EER)
* EB.DataAccess.FRead(FN.AC,Y.CR.AC,R.AC.REC,FV.AC,AC.EER)
        Y.AC.COM = R.AC.REC<AC.CO.CODE>
    END
    ELSE
        CALL F.READ(FN.AC,Y.DR.AC,R.AC.REC,FV.AC,AC.EER)
* EB.DataAccess.FRead(FN.AC,Y.DR.AC,R.AC.REC,FV.AC,AC.EER)
        Y.AC.COM = R.AC.REC<AC.CO.CODE>
    END
    COM.CD.VAL = Y.AC.COM
        
        
RETURN

END