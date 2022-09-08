* @ValidationCode : MjotMjE2ODQ5NTA3OkNwMTI1MjoxNjMyNzI5OTcwNjg5OnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 27 Sep 2021 14:06:10
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE GB.MBL.H.DEBIT.CARD.REQ.AUTH
*
*Subroutine Description:
* This routine is used for writing CARD information to the Templates.
*
*Attached To    : version(MBL.H.DEBIT.CARD.REQ,MBL.INPUT)
*Attached As    : BEFORE AUTH ROUTINE
*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    !** Simple AUTHORISE routine for template MBL.H.DEBIT.CARD.REQ, MBL.H.CARD.REQ.LIST
*-----------------------------------------------------------------------------
* Modification History:
*-----------------------------------------------------------------------------
* MD SHIBLI MOLLAH FDS -- on 07TH AUG 2021
*
*-----------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_ENQUIRY.COMMON
    $INSERT I_F.MBL.H.DEBIT.CARD.REQ
    $INSERT I_F.MBL.H.CARD.REQ.LIST
    
    $USING EB.TransactionControl
    $USING EB.DataAccess
    $USING EB.SystemTables
* $INSERT I_F.DATES
* $USING EB.Utility


*** </region>
*-----------------------------------------------------------------------------

*** <region name= MAIN PROCESS LOGIC>
*** <desc>Main process logic</desc>
    GOSUB INIT
*
    GOSUB PROCESS
RETURN
*** </region>

*** <region name= INITIALISE>
*** <desc>Initialise</desc>
INIT:
*
    Y.ACC.NO = EB.SystemTables.getIdNew()
   
*    FN.DATES = 'F.DATES'
*    F.DATES = ''

    FN.CRD.LIST = 'F.MBL.H.CARD.REQ.LIST'
    F.CRD.LIST = ''

    FN.CRD.REQ = 'F.MBL.H.DEBIT.CARD.REQ'
    F.CRD.REQ = ''
    
    EB.DataAccess.Opf(FN.CRD.LIST,F.CRD.LIST)
    EB.DataAccess.Opf(FN.CRD.REQ,F.CRD.REQ)
*  EB.DataAccess.Opf(FN.DATES,F.DATES)

RETURN
*** </region>
*-----------------------------------------------------------------------------

*** <region name= PROCESS>
*** <desc>Process</desc>
PROCESS:
*
    Y.COMPANY = EB.SystemTables.getIdCompany()
*
*  EB.DataAccess.FRead(FN.DATES,Y.COMPANY,REC.COMPANY,F.DATES,ERR.DATES)
* Y.TODAY = REC.COMPANY<EB.DAT.TODAY>
    Y.TODAY = EB.SystemTables.getToday()
    !Y.TODAY = 20150803
*
    EB.DataAccess.FRead(FN.CRD.REQ,Y.ACC.NO,REC.CRD.REQ,F.CRD.REQ,ERR.CRD.REQ)
    Y.CRD.REQ.REC.STATUS =REC.CRD.REQ<DR.CARD.RECORD.STATUS>
*
    Y.REC.STATUS = EB.SystemTables.getRNew(DR.CARD.RECORD.STATUS)
    Y.CURR1 = EB.SystemTables.getRNew(DR.CARD.CURR.NO)

*
    EB.DataAccess.FRead(FN.CRD.LIST,Y.TODAY,REC.CRD.LIST,F.CRD.LIST,ERR.CRD.LIST)

    IF Y.REC.STATUS EQ 'RNAU' THEN
        ARR = REC.CRD.LIST<CARD.LIST.ACCOUNT.NUMBER>
        
        Y.TOTAL.REC= DCOUNT(ARR,@VM)
        Y.NEW.REC.COUNT = Y.TOTAL.REC - 1

        FIND Y.ACC.NO IN ARR SETTING Y.POS1,Y.POS2 THEN

            Y.B = Y.POS2 - 1
            FOR I =1 TO Y.B

* Y.ARR.NEW<CARD.LIST.ACCOUNT.NUMBER,I> = ARR<CARD.LIST.ACCOUNT.NUMBER,I>
                Y.ARR.NEW<CARD.LIST.ACCOUNT.NUMBER,I> = ARR<CARD.LIST.ACCOUNT.NUMBER,I>

            NEXT I
            FOR J = Y.POS2 TO Y.NEW.REC.COUNT

                Y.ARR.NEW<CARD.LIST.ACCOUNT.NUMBER,J> = ARR<CARD.LIST.ACCOUNT.NUMBER,J+1>

            NEXT J
            WRITE Y.ARR.NEW TO F.CRD.LIST,Y.TODAY

        END
    END
    ELSE
        IF Y.CURR1 EQ 1 THEN

            Y.COUNT = DCOUNT(REC.CRD.LIST,@VM) +1

            REC.CRD.LIST<CARD.LIST.ACCOUNT.NUMBER,Y.COUNT> = Y.ACC.NO
            WRITE REC.CRD.LIST TO F.CRD.LIST,Y.TODAY
        END
    END
    
*--------------------------------TRACER-----------------------------------------
    WriteData = 'REC.CRD.REQ: ':REC.CRD.REQ:' ':'Y.CRD.REQ.REC.STATUS: ':Y.CRD.REQ.REC.STATUS:' REC.STATUS: ':Y.REC.STATUS:" Y.CURR1 ":Y.CURR1:" ":Y.ACC.NO:" REC.CRD.LIST :":REC.CRD.LIST:" Y.POS1: ":Y.POS1:"Y.POS2: ":Y.POS2:" Y.NEW.REC.COUNT: ":Y.NEW.REC.COUNT
    FileName = 'SHIBLI_CARD_SEP.21.txt'
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
*--------------------------------TRACER END-----------------------------------------

RETURN
*** </region>
*-----------------------------------------------------------------------------
END
