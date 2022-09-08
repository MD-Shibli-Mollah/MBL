* @ValidationCode : MjotNjE3ODE2NDUxOkNwMTI1MjoxNjM0Nzk4NDY2OTgxOnVzZXI6LTE6LTE6MDowOmZhbHNlOk4vQTpERVZfMjAxNzEwLjA6LTE6LTE=
* @ValidationInfo : Timestamp         : 21 Oct 2021 12:41:06
* @ValidationInfo : Encoding          : Cp1252
* @ValidationInfo : User Name         : user
* @ValidationInfo : Nb tests success  : N/A
* @ValidationInfo : Nb tests failure  : N/A
* @ValidationInfo : Rating            : N/A
* @ValidationInfo : Coverage          : N/A
* @ValidationInfo : Strict flag       : N/A
* @ValidationInfo : Bypass GateKeeper : false
* @ValidationInfo : Compiler Version  : DEV_201710.0

SUBROUTINE MBL.ENQ.ARR.CUS.R
*-----------------------------------------------------------------------------
**
*------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
* Modification History 1 :
* 20/09/2021 -                        Developed by   - MD. SHIBLI MOLLAH,
*                                                 FDS Bangladesh Limited
*------------------------------------------------------------------------------

    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_GTS.COMMON
        
    $USING EB.Reports
    $USING EB.DataAccess
    $USING EB.SystemTables

    $USING AA.Framework
    $USING AA.Customer
    
    GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
    
RETURN

*****
INIT:
*****

    FN.AA.ARR.CUS = 'F.AA.ARR.CUSTOMER'
    F.AA.ARR.CUS = ''
                    
    Y.AA.ID = EB.Reports.getOData()
    
RETURN


**********
OPENFILES:
**********
    EB.DataAccess.Opf(FN.AA.ARR.CUS, F.AA.ARR.CUS)
    
RETURN

********
PROCESS:
********
    SEL.CMD.ARR.CUS = "SELECT ":FN.AA.ARR.CUS:" WITH ID.COMP.1 EQ ":Y.AA.ID
             
    EB.DataAccess.Readlist(SEL.CMD.ARR.CUS, SEL.LIST, '', NO.OF.REC, SystemReturnCode)
    LOOP
        REMOVE Y.ARR.ID FROM SEL.LIST SETTING POS
        
    WHILE Y.ARR.ID:POS
        EB.DataAccess.FRead(FN.AA.ARR.CUS, Y.ARR.ID, REC.AA.CUS, F.AA.ARR.CUS, ERR.ARR.CUS)
        Y.CUS.ID = REC.AA.CUS<AA.Customer.Customer.CusCustomer>
       
        CONVERT @SM TO @VM IN Y.CUS.ID
        Y.DCOUNT = DCOUNT(Y.CUS.ID,@VM)
    REPEAT
    
    EB.Reports.setOData(Y.DCOUNT)
    
    
*******--------------------------TRACER------------------------------------------------------------------------------
*    WriteData = Y.ARR.ID:" ":SEL.CMD.ARR.CUS:" ":NO.OF.REC:" ":REC.AA.CUS: " ":Y.CUS.ID:" Y.DCOUNT ":Y.DCOUNT
*    FileName = 'ARR.NUM.CUS.txt'
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

*******--------------------------TRACER-END--------------------------------------------------------*********************
    
RETURN



END