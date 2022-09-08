* @ValidationCode : MjoxNzE3NjA0ODAwOkNwMTI1MjoxNjMyMjI0OTgwODk2OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Sep 2021 17:49:40
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
PROGRAM MBL.UNAUTH.DELET.ACTIVITY
*-----------------------------------------------------------------------------
*

    $INSERT  I_COMMON
    $INSERT  I_EQUATE
      
    $USING  EB.SystemTables
    $USING  EB.DataAccess
    $USING  AA.Framework
    $USING  EB.Updates
    $USING  EB.Interface
*-----------------------------------------------------------------------------
* Modification History :
*Developed By: Jobayer Hossain
*FDS
*Date :08/24/2021
*-----------------------------------------------------------------------------

    FN.AA.ARRANGEMENT.ACTIVITY ='FBNK.AA.ARRANGEMENT.ACTIVITY$NAU'
    F.AA.ARRANGEMENT.ACTIVITY= ''
    
    Y.COM = EB.SystemTables.getIdCompany()
    SEL.CMD.ARR = 'SELECT ':FN.AA.ARRANGEMENT.ACTIVITY :' AND CO.CODE EQ ': Y.COM

    EB.DataAccess.Readlist(SEL.CMD.ARR, SEL.LIST, '', NO.OF.REC, SystemReturnCode)
    LOOP
        REMOVE Y.ACTIVITY.ID FROM SEL.LIST SETTING POS
    WHILE Y.ACTIVITY.ID:POS

        OFS.SOURCE.PR = 'MBL.PRE.OFS'
        OFS.ERR.PR = ''
        OFS.MSG.ID.PR = ''
        OFS.STRING.PR = ''
        OFS.STRING.PR ='AA.ARRANGEMENT.ACTIVITY,/D/PROCESS,SHIBLI02/Moh@mmeD123456/':Y.COM:',':Y.ACTIVITY.ID


        EB.Interface.OfsCallBulkManager(OFS.SOURCE.PR, OFS.STRING.PR, OFS.RES.PR, TXN.VAL.PR)
        SENSITIVITY = ''


    REPEAT
    

*-----------------------------------------------------------------------------

END
