* @ValidationCode : MjotMTEwOTYzMDU3OTpDcDEyNTI6MTYyMTc2NTczNTY0NTp1c2VyOi0xOi0xOjA6MDpmYWxzZTpOL0E6REVWXzIwMTcxMC4wOi0xOi0x
* @ValidationInfo : Timestamp         : 23 May 2021 16:28:55
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0
SUBROUTINE OFS.TEST.2
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER

    $USING EB.Foundation
    $USING EB.Interface
    $USING EB.TransactionControl
*-----------------------------------------------------------------------------
    OfsMsgId = 'SECTOR,/I//0,INPUTT/123456,8888,SHORT.NAME=TECHNO,DESCRIPTION:1=TECHNOLOGY,DESCRIPTION:2:1=IT,RISK.EXPO.TYPE=NULL'
    OfsSourceId = 'GENERIC.OFS.PROCESS'
    OfsRec = ''
    Options = ''
    EB.Interface.OfsPostMessage(OfsMsgId, OfsRec, OfsSourceId, Options)
    EB.TransactionControl.JournalUpdate('')
       
    OFS.SOURCE = 'MBL.PRE.OFS'
                
*  Y.MESSAGE="FUNDS.TRANSFER,MBL.AA.MSS.FUND.OFS/I/PROCESS//0,//":Y.COM:",,TRANSACTION.TYPE=ACLP,DEBIT.ACCT.NO=":Y.DEBIT.ACCT.ID:",DEBIT.CURRENCY=":EB.SystemTables.getLccy():",DEBIT.AMOUNT=":Y.UNC.AMT:",DEBIT.VALUE.DATE=":EB.SystemTables.getToday():",CREDIT.VALUE.DATE=":EB.SystemTables.getToday():",CREDIT.ACCT.NO=":Y.CR.ACC.NUM:",ORDERING.BANK=":Y.ORD.BNK:",DEBIT.THEIR.REF=":THEIR.REF:",CREDIT.THEIR.REF=":THEIR.REF
                   
*  Y.MESSAGE = "FUNDS.TRANSFER,MBL.DFE.CHQ.CLG.OW.H/I/PROCESS,SHIBLI01/Moh@mmeD123//,/******/BD0010001///////////////,,TRANSACTION.TYPE:1:1="AC",DEBIT.ACCT.NO:1:1="BDT1402500010001",DEBIT.CURRENCY:1:1="BDT",DEBIT.AMOUNT:1:1="500017",DEBIT.VALUE.DATE:1:1="202104",CREDIT.ACCT.NO:1:1="1111000000255",PAYMENT.DETAILS:1:1="AB-1233",CHARGES.ACCT.NO:1:1="1111000000255",COMMISSION.CODE:1:1="DEBIT PLUS CHARGES",COMMISSION.TYPE:1:1="BACHCHGRV",LOCAL.REF:9:1="1233",LOCAL.REF:10:1="20210412",LOCAL.REF:29:1="121",LOCAL.REF:73:1="RV",LOCAL.REF:74:1="1234",LOCAL.REF:75:1="2"
* OfsRec = ''
* Options = ''
* EB.Interface.OfsPostMessage(Y.MESSAGE, OfsRec, OFS.SOURCE, Options)
    EB.Interface.OfsCallBulkManager(OFS.SOURCE, Y.MESSAGE, OFS.RES, TXN.VAL)
    SENSITIVITY = ''
                    
RETURN
END