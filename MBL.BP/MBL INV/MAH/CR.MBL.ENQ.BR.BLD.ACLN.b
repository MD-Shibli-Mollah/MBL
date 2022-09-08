* @ValidationCode : MjotMTMzMzYwNjA5OTpDcDEyNTI6MTYzMzUxNDAyMzA3Nzp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 06 Oct 2021 15:53:43
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

SUBROUTINE CR.MBL.ENQ.BR.BLD.ACLN(ENQ.DATA)

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON

    Y.FIELDS = ENQ.DATA<2>
    Y.POS = DCOUNT(Y.FIELDS,@VM) + 1
    ENQ.DATA<2,Y.POS> = 'PRODUCT.GROUP'
    ENQ.DATA<3,Y.POS> = 'EQ'
	ENQ.DATA<4,Y.POS> = 'MBL.WC.GRP.AC MBL.WC.STIM.AC MBL.CSHCR.HP.LN MBL.SOD.EMFS.LN MBL.SOD.FDR.LN MBL.SOD.GEN.LN MBL.SOD.SCHM.LN'
    
RETURN
END