{*******************************************************}
{                                                       }
{           Bill acceptor module                        }
{            according to the CCNET protocol            }
{                                                       }
{                                                       }
{*******************************************************}

unit UccNet;

interface

//error codes range from 100 to 199
//100 - com port error
//101 - device reset error
//102 - identification errors
//103 - status getting errors
//104 - errors in setting the list of accepted banknotes
//105 - privacy setting errors
//106 - banknote acceptance errors
//107 - banknote return errors
//108 - ASC errors
//109 - NSC errors
//110 - Device poll errors

{Header Types
        FF : 'Factory set:up and test',
        FE : 'Simple poll',
        FD : 'Address poll',
        FC : 'Address clash',
        FB : 'Address change',
        FA : 'Address random',
        F9 : 'Request polling priority',
        F8 : 'Request status',
        F7 : 'Request variable set',
        F6 : 'Request manufacturer id',
        F5 : 'Request equipment category id',
        F4 : 'Request product code',
        F3 : 'Request database version',
        F2 : 'Request serial number',
        F1 : 'Request software revision',
        F0 : 'Test solenoids',
        EF : 'Operate motors',
        EE : 'Test output lines',
        ED : 'Read input lines',
        EC : 'Read opto states',
        EB : 'Read last credit or error code',
        EA : 'Issue guard code',
        E9 : 'Latch output lines',
        E8 : 'Perform self:check',
        E7 : 'Modify inhibit status',
        E6 : 'Request inhibit status',
        E5 : 'Read buffered credit or error codes',
        E4 : 'Modify master inhibit status',
        E3 : 'Request master inhibit status',
        E2 : 'Request insertion counter',
        E1 : 'Request accept counter',
        E0 : 'Dispense coins',
        DF : 'Dispense change',
        DE : 'Modify sorter override status',
        DD : 'Request sorter override status',
        DC : 'One:shot credit',
        DB : 'Enter new PIN number',
        DA : 'Enter PIN number',
        D9 : 'Request payout high / low status',
        D8 : 'Request data storage availability',
        D7 : 'Read data block',
        D6 : 'Write data block',
        D5 : 'Request option flags',
        D4 : 'Request coin position',
        D3 : 'Power management control',
        D2 : 'Modify sorter paths',
        D1 : 'Request sorter paths',
        D0 : 'Modify payout absolute count',
        CF : 'Request payout absolute count',
        CE : 'Empty payout',
        CD : 'Request audit information block',
        CC : 'Meter control',
        CB : 'Display control',
        CA : 'Teach mode control',
        C9 : 'Request teach status',
        C8 : 'Upload coin data',
        C7 : 'Configuration to EEPROM',
        C6 : 'Counters to EEPROM',
        C5 : 'Calculate ROM checksum',
        C4 : 'Request creation date',
        C3 : 'Request last modification date',
        C2 : 'Request reject counter',
        C1 : 'Request fraud counter',
        C0 : 'Request build code',
        BF : 'Keypad control',
        BE : 'Request payout status',
        BD : 'Modify default sorter path',
        BC : 'Request default sorter path',
        BB : 'Modify payout capacity',
        BA : 'Request payout capacity',
        B9 : 'Modify coin id',
        B8 : 'Request coin id',
        B7 : 'Upload window data',
        B6 : 'Download calibration info',
        B5 : 'Modify security setting',
        B4 : 'Request security setting',
        B3 : 'Modify bank select',
        B2 : 'Request bank select',
        B1 : 'Handheld function',
        B0 : 'Request alarm counter',
        AF : 'Modify payout float',
        AE : 'Request payout float',
        AD : 'Request thermistor reading',
        AC : 'Emergency stop',
        AB : 'Request hopper coin',
        AA : 'Request base year',
        A9 : 'Request address mode',
        A8 : 'Request hopper dispense count',
        A7 : 'Dispense hopper coins',
        A6 : 'Request hopper status',
        A5 : 'Modify variable set',
        A4 : 'Enable hopper',
        A3 : 'Test hopper',
        A2 : 'Modify inhibit and override registers',
        A1 : 'Pump RNG',
        A0 : 'Request cipher key',
        9F : 'Read buffered bill events',
        9E : 'Modify bill id',
        9D : 'Request bill id',
        9C : 'Request country scaling factor',
        9B : 'Request bill position',
        9A : 'Route bill',
        99 : 'Modify bill operating mode',
        98 : 'Request bill operating mode',
        97 : 'Test lamps',
        96 : 'Request individual accept counter',
        95 : 'Request individual error counter',
        94 : 'Read opto voltages',
        93 : 'Perform stacker cycle',
        92 : 'Operate bi:directional motors',
        91 : 'Request currency revision',
        90 : 'Upload bill tables',
        8F : 'Begin bill table upgrade',
        8E : 'Finish bill table upgrade',
        8D : 'Request firmware upgrade capability',
        8C : 'Upload firmware',
        8B : 'Begin firmware upgrade',
        8A : 'Finish firmware upgrade',
        89 : 'Switch encryption code',
        88 : 'Store encryption code',
        87 : 'Set accept limit',
        86 : 'Dispense hopper value',
        85 : 'Request hopper polling value',
        84 : 'Emergency stop value',
        83 : 'Request hopper coin value',
        82 : 'Request indexed hopper dispense count',
        81 : 'Read barcode data',
        80 : 'Request money in',
        7F : 'Request money out',
        7E : 'Clear money counters',
        7D : 'Pay money out',
        7C : 'Verify money out',
        7B : 'Request activity register',
        7A : 'Request error status',
        79 : 'Purge hopper',
        78 : 'Modify hopper balance',
        77 : 'Request hopper balance',
        76 : 'Modify cashbox value',
        75 : 'Request cashbox value',
        74 : 'Modify real time clock',
        73 : 'Request real time clock',
        72 : 'Request USB id',
        71 : 'Switch baud rate',
        70 : 'Read encrypted events',
        6F : 'Request encryption support',
        6E : 'Switch encryption key',
        6D : 'Request encrypted hopper status',
        6C : 'Request encrypted monetary id',
        6B : 'Operate escrow',
        6A : 'Request escrow status',
        69 : 'Data stream',
        68 : 'Request service status',
        4 :  'Request comms revision',
        3 :  'Clear comms status variables',
        2 :  'Request comms status variables',
        1 :  'Reset device',
        0 :  'Reply'
}


const
  POLYNOM = $01021;

  B10   =   4;  //00000100
  B50   =   8;  //00001000
  B100  =   16; //00010000
  B500  =   32; //00100000
  B1000 =   64; //01000000
  B5000 =   128;//10000000

type

  Tnominal = record
    B10:Boolean;
    B50:Boolean;
    B100:Boolean;
    B500:Boolean;
    B1000:Boolean;
    B5000:Boolean;
  end;

  TProcessMessage = procedure(Error:integer;Mess:string) of object;
  TPollingBill    = procedure(Nominal:word;var CanLoop:boolean) of object;

  TCashCodeBillValidatorCCNET = class(TObject)
    constructor Create();
    destructor Destroy(); override;
  private
    FComFile  : THandle;  // Pointer to com port
    FAscii: string;
    FPollStatus:Boolean;
    FProcessMesage:TProcessMessage;
    FPolingBill:TPollingBill;
    FCanPollingLoop:Boolean; // Sign of the polling cycle
    FNamberComPort:Byte;
    FComConnected:Boolean;
    FCommand:Array[0..255] of Byte; //Command
    FLengthCommand:Byte;
    FAnswer:Array[0..255] of Byte;  //Answer
    FLengthAnswer:Byte;
    FData:Array[0..255] of Byte;    //Response Data
    FLengthData:Byte;
    procedure ProcessMessage(CodeMess:integer;Mess:string);
    procedure PolingBill(Nominal:word;var CanLoop:boolean);
    //Execution of the generated command
    procedure ProcessComand();
    // Excuting the command
    procedure SendPacket(Command:Byte;Data:Array of Byte);
    //Parse Answer
    procedure ParseAnswer();
    // Clear Command
    procedure ClearCommand();
    // Clear Answer
    procedure ClearAnswer();
    // Clear Data
    procedure ClearData();
  public
    //Инициализация com порта
    function OpenComPort:Boolean;
    // Закрываем ком порт
    procedure CloseComPort();
    // Основные команды валидатора
    function Reset():Boolean; // Сброс купюро-приемника
    function Identification(var Name,Namber:string):Boolean;   //Номер и название купюроприемника
    function GetStatus(var Nominal,Security:TNominal):Boolean; //Получение списка принимаемых купюр
    function EnableBillTypes(Nominal:TNominal):Boolean;        //Установка прнимаемых купюр
    function SetSecurity(Nominal:TNominal):Boolean;            //Установка повышенного контроля для определенных купюр
    function Stack():Boolean;                                  //Принимаем купюру ESCROW режим
    function Return():Boolean;                                 //Возврат купюры   ESCROW режим
    function SendASC():Boolean;                                //Подтверждаем получение
    function SendNSC():Boolean;                                //Не подтверждааем получение
    function Poll():boolean;                                   //Опрос устройства
    function PollingLoop(Sum:word;TimeLoop:LongWord):Word;
    function TestSolenoid:Boolean;
    function Equipment(var number:string):boolean;
    function Serial(var number:string):boolean;
    function Init():boolean;
    procedure  StartPoll;

    // Property for setting the com port number
    property NamberComPort:Byte read FNamberComPort write FNamberComPort;
    // Event on any messages
    property OnProcessMessage:TProcessMessage read FProcessMesage write FProcessMesage;
    // Sign of establishing a connection with the com port
    property ComConnected:Boolean read FComConnected;
    // An event that occurs when a banknote is received, we can interrupt the work through the CanLoop variable
    property OnPolingBill:TPollingBill read FPolingBill write FPolingBill;
    // Property checking/setting the poll cycle
    property CanPollingLoop:Boolean read FCanPollingLoop write FCanPollingLoop;
  end;

function GetCRC16(InData: array of byte; DataLng: word): word;

// Checking a given bit for 1
function IsBitSet(Value: Byte; BitNum : byte): boolean;

// Setting the specified bit to 1
function BitOn(const val: Byte; const TheBit: byte): Byte;

// Setting the specified bit to 0
function BitOff(const val: Byte; const TheBit: byte): Byte;

implementation

{ TCashCodeBillValiddator }

Uses SysUtils,Windows,DateUtils;

procedure  TCashCodeBillValidatorCCNET.StartPoll;
begin
  if not FComConnected then raise Exception.Create('RESET FAILED');
  try
    FPollStatus:=True;
    while (1=1) or (FPollStatus=false) do
    begin
      SendPacket($E5,[]);
      ProcessMessage(205,'->RETURN');
      ProcessComand();
    end;
  except
    on E:Exception do
    begin
      ProcessMessage(107,E.Message);
    end;
  end;
end;

procedure TCashCodeBillValidatorCCNET.ClearAnswer;
begin
  FillChar(FAnswer, SizeOf(FAnswer),0);
  FLengthAnswer:=0;
end;

procedure TCashCodeBillValidatorCCNET.ClearCommand;
begin
  FillChar(FCommand, SizeOf(FCommand),0);
  FLengthCommand:=0;
end;

procedure TCashCodeBillValidatorCCNET.ClearData;
begin
  FillChar(FData, SizeOf(FData),0);
  FLengthData:=0;
end;

procedure TCashCodeBillValidatorCCNET.CloseComPort();
begin
  if FComFile <> INVALID_HANDLE_VALUE
  then CloseHandle(FComFile);
  FComFile := INVALID_HANDLE_VALUE;
  FComConnected:=false;
end;

constructor TCashCodeBillValidatorCCNET.Create;
begin
  inherited;
end;

destructor TCashCodeBillValidatorCCNET.Destroy;
begin
  inherited;
  CloseComPort;
end;

function TCashCodeBillValidatorCCNET.TestSolenoid: Boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($F0,[]);
   ProcessMessage(205,'->RETURN');
   ProcessComand();
   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(107,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.OpenComPort: Boolean;
const
  RxBufferSize = 256;
  TxBufferSize = 256;
var
  DeviceName: array[0..80] of Char;
  DCB: TDCB;
  CommTimeouts: TCommTimeouts;
begin
  try
    result:=true; //We hope for the best
    FComConnected:=True;

    if FNamberComPort = 0
    then raise Exception.Create('No COM port number set');

    StrPCopy(DeviceName, 'Com'+IntToStr(FNamberComPort)+':');
    FComFile := CreateFile(DeviceName, GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    if FComFile = INVALID_HANDLE_VALUE
    then raise Exception.Create('Failed to open COM port');

    if not SetupComm(FComFile, RxBufferSize, TxBufferSize)
    then raise Exception.Create('Failed to set COM port buffer');

    if not GetCommState(FComFile, DCB)
    then raise Exception.Create('Failed to read COM port parameters');

    // Задаем параемтры порта
    DCB.BaudRate:=9600;
    DCB.ByteSize:=8;
    DCB.Parity:=noparity;
    DCB.StopBits:=ONESTOPBIT;
    DCB.Flags:=DTR_CONTROL_ENABLE;

    if not SetCommState(FComFile, DCB)
    then raise Exception.Create('Failed to set COM port parameters');

    CommTimeouts.ReadIntervalTimeout         := 400;
    CommTimeouts.ReadTotalTimeoutMultiplier  := 0;
    CommTimeouts.ReadTotalTimeoutConstant    := 400;
    CommTimeouts.WriteTotalTimeoutMultiplier := 0;
    CommTimeouts.WriteTotalTimeoutConstant   := 400;

    if not SetCommTimeouts(FComFile, CommTimeouts)
    then raise Exception.Create('Failed to set COM port timeouts');

  except
    on E:Exception do
    begin
      result:=False;
      FComConnected:=False;
      ProcessMessage(100,E.Message);
    end;
  end;
end;

procedure TCashCodeBillValidatorCCNET.ParseAnswer;
var
  CRC16:array[0..1] of Byte;
  CRCWord:Word;
  i:Integer;
  s:string;
  d: array[1..254] of byte;
begin
  ClearData();
  if FLengthAnswer >= FLengthCommand then
  begin
     CopyMemory(@FData,@FAnswer,FLengthAnswer);
     for i := FLengthCommand+1 to FLengthAnswer-2 do
     begin
       s:= s + chr(FAnswer[i]);
     end;
     FAscii:=s;
     FLengthData:=FLengthAnswer;
  end;
end;

procedure TCashCodeBillValidatorCCNET.PolingBill(Nominal: word;
  var CanLoop: boolean);
begin
  if Assigned(FPolingBill) then FPolingBill(Nominal, CanLoop);
end;

function TCashCodeBillValidatorCCNET.Poll: boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($FE,[]);
   ProcessMessage(201,'->SIMPLE POLL');
   ProcessComand();
   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(110,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.PollingLoop(Sum: word;
  TimeLoop: LongWord): Word;
var
  StartTime:TDateTime;
  FirsByte,SEcondByte:Byte;
  BillNominal:word;
begin
  result:= 0;
  StartTime:=Now(); // Получаем текущее время
  FCanPollingLoop:=true;
  while FCanPollingLoop do
  begin
    if Poll() then  // Если опрос прошел успешно
    begin
      FirsByte  :=FData[0];
      SEcondByte:=FData[1];

      case FirsByte of
        $10,$11,$12 : begin
                        ProcessMessage(213,'Power on after commands');
                        FCanPollingLoop:=false;
                      end;
        $13:begin
               ProcessMessage(214,'Initialization');
             end;
        $14:begin
               ProcessMessage(215,'Waiting for banknotes');
             end;
        $15:begin
               ProcessMessage(216,'Accept');
             end;
        $19:begin
               ProcessMessage(217,'Not available, waiting to be initialized');
             end;
        $41:begin
               ProcessMessage(218,'Full cassette');
               reset();
               FCanPollingLoop:=false;
             end;
         $42:begin
               ProcessMessage(219,'No cassette');
               reset();
               FCanPollingLoop:=false;
             end;
         $43:begin
               ProcessMessage(220,'The banknote has been jammed');
               reset();
               FCanPollingLoop:=false;
             end;
         $44:begin
               ProcessMessage(221,'Cassette jammed 0_o');
               reset();
               FCanPollingLoop:=false;
             end;
         $45:begin
               ProcessMessage(222,'SENTRY !!!! CROOKS !!!');
               reset();
               FCanPollingLoop:=false;
             end;
        $47:begin
              ProcessMessage(223,'Hardware failure');
              case SEcondByte of
                $50:ProcessMessage(224,'Stack_motor_falure');
                $51:ProcessMessage(225,'Transport_speed_motor_falure');
                $52:ProcessMessage(226,'Transport-motor_falure');
                $53:ProcessMessage(227,'Aligning_motor_falure');
                $54:ProcessMessage(228,'Initial_cassete_falure');
                $55:ProcessMessage(229,'Optical_canal_falure');
                $56:ProcessMessage(230,'Magnetical_canal_falure');
                $5F:ProcessMessage(231,'Capacitance_canal_falure');
              end;
            end;
        $1C:begin
              ProcessMessage(232,'Refusal of admission');
              case SEcondByte of
                $60:ProcessMessage(233,'Insertion_error');
                $61:ProcessMessage(234,'Dielectric_error');
                $62:ProcessMessage(235,'Previously_inserted_bill_remains_in_head');
                $63:ProcessMessage(236,'Compensation__factor_error');
                $64:ProcessMessage(237,'Bill_transport_error');
                $65:ProcessMessage(238,'Identification_error');
                $66:ProcessMessage(239,'Verification_error');
                $67:ProcessMessage(240,'Optic_sensor_error');
                $68:ProcessMessage(241,'Return_by_inhibit_error');
                $69:ProcessMessage(242,'Capacistance_error');
                $6A:ProcessMessage(243,'Operation_error');
                $6C:ProcessMessage(244,'Length_error');
              end;
            end;
        $80  Begin
				ProcessMessage(245,'Deposit'); // I didn’t finish it, because I don’t see practical application, in general, whoever needs it will finish it
             end;
         $81:begin
               ProcessMessage(246,'Packing');
               case SEcondByte of
                 2:BillNominal:=10;
                 3:BillNominal:=50;
                 4:BillNominal:=100;
                 5:BillNominal:=500;
                 6:BillNominal:=1000;
                 7:BillNominal:=5000;
               end;
               StartTime:=Now(); // Reset the time to wait for the next bill
               SendASC(); // Acknowledge reception
               result:=result+BillNominal;
               PolingBill(BillNominal,FCanPollingLoop); // Give a message about the accepted banknote
               if result>=Sum then
               begin
                 FCanPollingLoop:=False; // The amount has accumulated, we finish begging
                 ProcessMessage(247,'Did the required amount');
               end;
             end;
         $82:begin
               ProcessMessage(248,'Bill return');
             end;
      end;
      Sleep(100);
    end;

    if SecondsBetween(Now(),StartTime)> TimeLoop then
    begin
      FCanPollingLoop:=false;
      ProcessMessage(249,'We complete the work on the timeout for accepting a bill');
    end;
  end;
end;

procedure TCashCodeBillValidatorCCNET.ProcessComand;
var
  BytesWritten:Dword;
  BytesRead:Dword;
  Errs:Dword;
  ComStat:TComStat;
begin
  ClearCommError(FComFile,Errs,@ComStat);
  if ComStat.cbInQue > 0 then
  begin
    if not PurgeComm(FComFile, PURGE_TXCLEAR or PURGE_RXCLEAR) then
    raise Exception.Create('COM FAILED');
  end;

  if not WriteFile(FComFile, FCommand, FLengthCommand, BytesWritten, nil)
  then Exception.Create('COM WRITE FAILED');

  if BytesWritten <> FLengthCommand
  then Exception.Create('COM WRITE FAILED');

  if not ClearCommError(FComFile,Errs,@ComStat)
  then Exception.Create('COM CLEAR FAILED');

  ClearAnswer();
  if not ReadFile(FComFile, FAnswer, SizeOf(FAnswer), BytesRead, nil) then
    Exception.Create('COM READ FAILED')
  else
    begin
      FLengthAnswer:=BytesRead;
      ParseAnswer();
    end;
end;

procedure TCashCodeBillValidatorCCNET.ProcessMessage(CodeMess: integer; Mess: string);
begin
  if Assigned(FProcessMesage) then FProcessMesage(CodeMess, Mess);
end;

function TCashCodeBillValidatorCCNET.Reset():Boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($30,[]);
   ProcessMessage(204,'->RESET');
   ProcessComand();

   if FData[0] = $FF then
   begin
     ProcessMessage(202,'<-NSC');
     raise Exception.Create('NAK FAILED (NAK)')
   end;

   if FData[0] = $00
   then ProcessMessage(203,'<-ASC');

   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(101,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.Return: Boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($36,[]);
   ProcessMessage(205,'->RETURN');
   ProcessComand();
   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(107,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.EnableBillTypes(
  Nominal: TNominal): Boolean;
var
  BillTypesByte:Byte;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   BillTypesByte:=0;
   if Nominal.B10 then BillTypesByte:=BillTypesByte+B10;
   if Nominal.B50 then BillTypesByte:=BillTypesByte+B50;
   if Nominal.B100 then BillTypesByte:=BillTypesByte+B100;
   if Nominal.B500 then BillTypesByte:=BillTypesByte+B500;
   if Nominal.B1000 then BillTypesByte:=BillTypesByte+B1000;
   if Nominal.B5000 then BillTypesByte:=BillTypesByte+B5000;



   SendPacket($34,[0,0,BillTypesByte,0,0,0]);
   ProcessMessage(206,'->ENABLE BILL TYPES');
   ProcessComand();

   if FData[0] = $FF then
   begin
     ProcessMessage(202,'<-NSC');
     raise Exception.Create('FAILED (NAK)')
   end;

   if FData[0] = $00
   then ProcessMessage(203,'<-ASC');

   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(104,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.SetSecurity(Nominal: TNominal): Boolean;
var
  BillTypesByte:Byte;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');

   BillTypesByte:=0;

   if Nominal.B10 then BillTypesByte:=BillTypesByte+B10;
   if Nominal.B50 then BillTypesByte:=BillTypesByte+B50;
   if Nominal.B100 then BillTypesByte:=BillTypesByte+B100;
   if Nominal.B500 then BillTypesByte:=BillTypesByte+B500;
   if Nominal.B1000 then BillTypesByte:=BillTypesByte+B1000;
   if Nominal.B5000 then BillTypesByte:=BillTypesByte+B5000;


   SendPacket($32,[0,0,BillTypesByte]);
   ProcessMessage(207,'->SET SECURITY');
   ProcessComand();

   if FData[0] = $FF then
   begin
     ProcessMessage(202,'<-NSC');
     raise Exception.Create('(NAK)')
   end;

   if FData[0] = $00
   then ProcessMessage(203,'<-ASC');

   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(105,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.Stack: Boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($35,[]);
   ProcessMessage(208,'->STACK');
   ProcessComand();
   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(106,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.Identification(var Name,Namber:string):Boolean;
var
  i:integer;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($F6,[]);
   ProcessComand();
   ProcessMessage(209,'->IDENTIFICATION');
   Name:=FAscii;
   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(102,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.Init: boolean;
begin
   try
     if not FComConnected then raise Exception.Create('RESET FAILED');
     SendPacket($B3,[]);
     ProcessComand();
     ProcessMessage(210,'->INIT');
   except
    on E:Exception do
    begin
      ProcessMessage(103,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.GetStatus(var Nominal,Security: Tnominal): Boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($31,[]);
   ProcessComand();
   ProcessMessage(210,'->GET STATUS');

   Nominal.B10 := IsBitSet(FData[2],2);
   Nominal.B50 := IsBitSet(FData[2],3);
   Nominal.B100 := IsBitSet(FData[2],4);
   Nominal.B500 := IsBitSet(FData[2],5);
   Nominal.B1000 := IsBitSet(FData[2],6);
   Nominal.B5000 := IsBitSet(FData[2],7);

   Security.B10 := IsBitSet(FData[5],2);
   Security.B50 := IsBitSet(FData[5],3);
   Security.B100 := IsBitSet(FData[5],4);
   Security.B500 := IsBitSet(FData[5],5);
   Security.B1000 := IsBitSet(FData[5],6);
   Security.B5000 := IsBitSet(FData[5],7);

   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(103,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.SendASC: Boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket(0,[]);
   ProcessMessage(211,'->ASC');
   try
     ProcessComand();
   except
   end;
   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(108,E.Message);
      result:=false;
    end;
  end;
end;


function TCashCodeBillValidatorCCNET.SendNSC: Boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($FF,[]);
   ProcessMessage(212,'->NSC');
   try
     ProcessComand();
   except
   end;

   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(109,E.Message);
      result:=false;
    end;
  end;
end;

procedure TCashCodeBillValidatorCCNET.SendPacket(Command: Byte; Data: array of Byte);
var
  CRC16:array[0..1] of Byte;
  CRCWord:Word;
  CheckSum:Byte;
  Modulo:Byte;
  i:Integer;
  CheckSum_pos:Integer;
begin
  ClearCommand();

  FLengthCommand:=7+Length(Data);
  CheckSum:=0;
  Modulo:=0;

  FCommand[0] := $02;
  FCommand[1] := $00;
  FCommand[2] := $01;
  FCommand[3] := Command;

  if Length(Data) <> 0 then
    CopyMemory(@FCommand[4],@Data,Length(Data));

  CheckSum_pos:=Length(Data)+4;
  for I := 0 to FLengthCommand -1 do
  begin
    CheckSum:=FCommand[i]+CheckSum;
  end;
  if CheckSum>255 then
  begin
     Modulo:=CheckSum div 256;
     Modulo:=(CheckSum * Modulo) - CheckSum;
     CheckSum:=Modulo;
     FCommand[CheckSum_pos]:=Modulo;
  end
  else
  begin
    FCommand[CheckSum_pos]:=256-CheckSum;
  end;
end;

function TCashCodeBillValidatorCCNET.Serial(var number: string): boolean;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($F2,[]);
   ProcessComand();
   ProcessMessage(209,'->Serial');
   number:=FASCII;
   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(102,E.Message);
      result:=false;
    end;
  end;
end;

function TCashCodeBillValidatorCCNET.Equipment(var number: string): boolean;
var
  i:integer;
begin
  try
   if not FComConnected then raise Exception.Create('RESET FAILED');
   SendPacket($F5,[]);
   ProcessComand();
   ProcessMessage(209,'->Equipment');
   number:=FASCII;
   result:=true;
  except
    on E:Exception do
    begin
      ProcessMessage(102,E.Message);
      result:=false;
    end;
  end;
end;

function GetCRC16(InData: array of byte; DataLng: word): word;
var
  i,TmpCRC: word;
  j: byte;
begin
  result:=0;
  for i:=0 to (DataLng-1) do
  begin
    TmpCRC:=result xor InData[i];
    for j:=0 to 7 do
    begin
      if (TmpCRC and $0001)<>0 then
      begin
        TmpCRC:=TmpCRC shr 1;
        TmpCRC:=TmpCRC xor POLYNOM;
      end
      else TmpCRC:=TmpCRC shr 1;
    end;
    result:=TmpCRC;
  end;
end;

function IsBitSet(Value: Byte; BitNum : byte): boolean;
begin
  result:=((Value shr BitNum) and 1) = 1
end;

function BitOn(const val: Byte; const TheBit: byte): Byte;
begin
  Result := val or (1 shl TheBit);
end;

function BitOff(const val: Byte; const TheBit: byte): Byte;
begin
  Result := val and ((1 shl TheBit) xor $FF);
end;

end.
