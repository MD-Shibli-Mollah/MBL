* @ValidationCode : MjotMjA4NTY5OTIwNTpDcDEyNTI6MTYyNTM5NDI4OTUwMDp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 04 Jul 2021 16:24:49
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE GB.MBL.E.CONV.CHRG.DESC
    
*-----------------------------------------------------------------------------
* Description: Conversion routine, converts AA ID to ACC ID
* Modification History :
* Author :      MD. Shibli Mollah -- FDS BD
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    
    $USING EB.Reports
    $USING ST.CompanyCreation
    $USING EB.DataAccess

    $USING EB.SystemTables

    GOSUB INIT
    GOSUB PROCESS
*
RETURN
*
*----------------------initialisation and opening of necessary files ---------------
*----
INIT:
*----
    FN.ARR = 'F.AA.ARRANGEMENT'
    F.ARR = ''
    Y.CHRG.CODE = EB.Reports.getOData()
   
*
RETURN
*
*-----------to get chrg amt if charge code is 'LCADVCHGS'------------
PROCESS:
*------
    EB.DataAccess.FRead(FN.LC, Y.TF.REF, LC.REC, F.LC, ERR.REC)
    Y.AC.LOC.REF = EB.SystemTables.getRNew(AA.Account.Account.AcLocalRef)
    
RETURN
END