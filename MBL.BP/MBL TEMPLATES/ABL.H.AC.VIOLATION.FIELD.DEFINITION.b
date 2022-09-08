* @ValidationCode : MjotMTAxNzM2NjE3NTpDcDEyNTI6MTYxMTA0NTE3OTgwMDp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 19 Jan 2021 14:32:59
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
*
*-----------------------------------------------------------------------------
* <Rating>-33</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE ABL.H.AC.VIOLATION.FIELD.DEFINITION
*-----------------------------------------------------------------------------
* Program Description
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.ACCOUNT       ;* Other Inserts required for Checkfiles, etc.
*-----------------------------------------------------------------------------
    GOSUB INITIALISE

    GOSUB DEFINE.FIELDS

RETURN
*
*-----------------------------------------------------------------------------
DEFINE.FIELDS:

    ID.F = "ACCOUNTNUMBER.DATE" ; ID.N = "25" ; ID.T = "A"
*
    Z=0
*

    Z+=1 ; F(Z) = "XX<AC.BAL.DATE.VL" ; N(Z) = "12" ; T(Z) = "D"
    Z+=1 ; F(Z) = "XX>AC.BAL.TXN.REF" ; N(Z) = "25" ; T(Z) = "A"
    Z+=1 ; F(Z) = "AC.BAL.ACI.ID" ; N(Z) = "25" ; T(Z) = "A"
    Z+=1 ; F(Z) = "FLAG" ; N(Z) = "1" ; T(Z)<2> = "Y_N"

REM > Z+=1 ; F(Z) = "XX.XX.FIELD.NAME" ; N(Z) = "35.2" ; T(Z) = "A"
REM > CHECKFILE(Z) = CHK.ACCOUNT
*
    V = Z + 9

RETURN
*
*-----------------------------------------------------------------------------
INITIALISE:

    MAT F = "" ; MAT N = "" ; MAT T = ""
    MAT CHECKFILE = "" ; MAT CONCATFILE = ""
    ID.CHECKFILE = "" ; ID.CONCATFILE = ""
*
* Define often used checkfile variables
*
    CHK.ACCOUNT = "ACCOUNT":FM:AC.SHORT.TITLE:FM:"L"

RETURN
*
*-----------------------------------------------------------------------------
*
END
