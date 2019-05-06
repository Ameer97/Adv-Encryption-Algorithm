Public Class Form1
    Dim discard As List(Of String) = New List(Of String)


    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim IpTable = Numbers_Generate(128, 1)
        Dim SBOX15 = importSBOX()
        Dim GenerateShiftMatrix = Numbers_Generate(8, 3, True)
        Dim InputKey1 As String = InputBox("Chaos parameter R")
        Dim InputKey2 As String = InputBox("Chaos parameter X")
        Dim chaosGenerateSTR = Join(Chaos_Table(InputKey1, InputKey2), "").Replace("0.", "")
        Dim chaosReordered128 = Ip_Procces_Encryption(IpTable, Mid(chaosGenerateSTR, 6, 128))
        Dim chaosBinary = KeySplit(Mid(chaosReordered128, 1, 8))
        Dim ChaosKey64 = Ip_Procces_Encryption(IpTable, chaosBinary)

        Dim Message

        Dim left
        Dim right


        Dim Org_Message = readFromFile()

        Dim Rounds As Integer = 15
        Dim encryptBits As String = ""
        Dim encrypt As String = ""
        Dim decryptBits As String = ""
        Dim decrypt As String = ""
        Dim watch As Stopwatch = Stopwatch.StartNew()

        watch.Start()
        For j = 1 To Org_Message.length Step 128

            Message = Mid(Org_Message, j, 128)
            Message = Ip_Procces_Encryption(IpTable, Message)

            For i = 0 To Rounds

                right = Mid(Message, 65, 64)
                left = Mid(Message, 1, 64)

                right = SplitAs8(right, SBOX15)
                left = SplitAs8(left, SBOX15)

                right = XorOperation(right, ChaosKey64, 64)
                left = MatrixFormShift(left, GenerateShiftMatrix)

                right = SerpentMode(right)

                Message = right & left
            Next
            encryptBits &= Message

        Next

        watch.Stop()
        TextBox1.Text = watch.Elapsed.TotalSeconds


        Dim File As IO.StreamWriter = New IO.StreamWriter("Encryption.txt")
        encrypt = BtoC_2(encryptBits)
        File.Write(encrypt)
        File.Close()


        watch = Stopwatch.StartNew()
        watch.Start()

        For j = 1 To encryptBits.Length Step 128

            Message = Mid(encryptBits, j, 128)

            For i = 0 To Rounds

                left = Mid(Message, 65, 64)
                right = Mid(Message, 1, 64)
                right = SerpentMode(right, True)

                right = XorOperation(right, ChaosKey64, 64)
                left = MatrixFormShift(left, GenerateShiftMatrix, True)

                right = D_SplitSBOX(right, SBOX15)     'Decrypt
                left = D_SplitSBOX(left, SBOX15)       'Decrypt

                Message = left & right
            Next

            decryptBits &= BtoC_2(INV_Order(IpTable, Message))          'Decrypt

        Next
        watch.Stop()
        TextBox2.Text = watch.Elapsed.TotalSeconds
        File = New IO.StreamWriter("Decryption.txt")
        File.Write(decryptBits)
        File.Close()

        Try
            RichTextBox1.Text = encrypt
        Catch ex As Exception
            RichTextBox1.Text = Mid(encrypt, 1, 40)
        End Try

    End Sub

    Function readFromFile()
        Dim OpenFileDialog1 As OpenFileDialog = New OpenFileDialog()

        If Not OpenFileDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Application.Exit()
        End If

        Dim FunDevRead As IO.StreamReader = New IO.StreamReader(OpenFileDialog1.FileName)
        Dim MyOrgString = FunDevRead.ReadToEnd()


        While (Not MyOrgString.Length Mod 16 = 0)
            MyOrgString += " "
        End While


        Dim spliter As String = ""
        Dim AscValue As Integer = 0
        Dim stored As String = ""



        For i = 1 To MyOrgString.Length()
            spliter = Mid(MyOrgString, i, 1)
            AscValue = Asc(spliter)
            stored += DtoB(AscValue, 8)
        Next

        Return stored

    End Function

    Function CtoB(MyOrgString)

        Dim spliter As String = ""
        Dim AscValue As Integer = 0
        Dim stored As String = ""

        While (Not MyOrgString.Length Mod 16 = 0)
            MyOrgString += " "
        End While


        For i = 1 To MyOrgString.Length()
            spliter = Mid(MyOrgString, i, 1)
            AscValue = Asc(spliter)
            stored += DtoB(AscValue, 8)
        Next

        Return stored
    End Function

    Function KeySplit(keySTR As String)

        Dim spliter As String = ""
        Dim AscValue As Integer = 0
        Dim stored As String = ""


        For i = 1 To keySTR.Length()
            spliter = Mid(keySTR, i, 1)
            AscValue = Asc(spliter)
            stored += DtoB(AscValue, 8)
        Next

        Return stored
    End Function


    Function DtoB(val As Integer, LenthOfReturnValue As Integer)
        Dim stack As String = ""

        While val <> 0
            If val Mod 2 = 0 Then
                stack = "0" + stack
            Else
                stack = "1" + stack
            End If
            val = val \ 2
        End While
        While stack.Length < LenthOfReturnValue
            stack = "0" + stack
        End While

        Return stack
    End Function



    Function importSBOX()

        Dim texte As String = System.IO.File.ReadAllText(FileReturnPath())

        Dim StrStack(15, 15) As Integer
        Dim col = 0
        Dim row = 0
        For i = 1 To texte.Length Step 8

            If Not contain_twoDinmation(StrStack, JustConvertToD(Mid(texte, i, 8))) Then

                StrStack(col, row) = JustConvertToD(Mid(texte, i, 8))
                row += 1

                If row = 16 Then
                    col += 1
                    row = 0
                End If

            End If
            If col = 15 AndAlso row = 15 Then
                Exit For
            End If

        Next

        Return StrStack
    End Function

    Function JustConvertToD(input As String)
        Dim stackInteger As Integer
        For i = 1 To input.Length
            stackInteger += Convert.ToInt32(Mid(input, i, 1)) * Math.Pow(2, input.Length - i)
        Next
        Return stackInteger
    End Function

    Function contain_twoDinmation(str As Integer(,), item As Integer)
        Dim check As Boolean = False
        For i = 0 To str.GetLength(0) - 1
            For j = 0 To str.GetLength(1) - 1

                If item = str(i, j) Then
                    check = True
                End If

            Next
        Next
        Return check
    End Function


    Function SplitAs8(str As String, TableSBox(,) As Integer)

        Dim Chunks(7) As String
        Dim chunksReplace(7) As String
        Dim counter As Integer = 0


        Dim spliterRow As String = ""
        Dim spliterColumn As String = ""

        Dim rows(15) As String
        Dim columns(15) As String

        Dim spliterRowInt As Integer = 0
        Dim spliterColumnInt As Integer = 0

        For i = 1 To str.Length Step 8

            Chunks(counter) = Mid(str, i, 8)

            spliterRow = Mid(Chunks(counter), 1, 4)
            spliterRowInt = JustConvertToD(spliterRow)
            spliterColumn = Mid(Chunks(counter), 5, 4)
            spliterColumnInt = JustConvertToD(spliterColumn)

            rows(counter) = spliterRowInt
            columns(counter) = spliterColumnInt

            chunksReplace(counter) = TableSBox(spliterColumnInt, spliterRowInt)
            counter += 1


        Next

        Dim FinalSide As String = ""
        For i = 0 To chunksReplace.Length - 1
            FinalSide += DtoB(chunksReplace(i), 8)
        Next

        Return FinalSide
    End Function



    Function D_SplitSBOX(EChunks As String, TableSBox(,) As Integer)
        Dim Chunks = SplitString(EChunks, 8)

        Dim equal_ChunkDecimal As Integer

        Dim DChunks(7) As String
        Dim counter As Integer = 0


        Dim spliterRow As String = ""
        Dim spliterColumn As String = ""

        Dim rows(15) As String
        Dim columns(15) As String

        Dim spliterRowInt As Integer = 0
        Dim spliterColumnInt As Integer = 0

        For i = 0 To Chunks.Length - 1
            equal_ChunkDecimal = JustConvertToD(Chunks(i))
            For r = 0 To 15
                For c = 0 To 15
                    If equal_ChunkDecimal = TableSBox(r, c) Then
                        spliterRowInt = r
                        spliterColumnInt = c

                        spliterRow = DtoB(spliterRowInt, 4)
                        spliterColumn = DtoB(spliterColumnInt, 4)
                        DChunks(i) = spliterColumn & spliterRow
                    End If
                Next
            Next
        Next

        Return Join(DChunks, "")
    End Function


    Function BtoC_2(val As String)

        Dim ArrString(val.Length) As String
        Dim StackInteger As Integer = 0
        Dim splitbit(8) As Integer
        Dim DtoC As String = ""
        Dim counter As Integer = 0
        For i = 1 To val.Length Step 8
            counter += 1
            StackInteger = 0
            ArrString(counter) = Mid(val, i, 8)
            For j = 1 To 8
                splitbit(j) = Convert.ToInt32(Mid(ArrString(counter), j, 1))
            Next


            For j = 1 To 8
                StackInteger += Math.Pow(2, 8 - j) * splitbit(j)
            Next
            DtoC += Chr(StackInteger)
        Next


        Return DtoC
    End Function


    Function Numbers_Generate(IndexPlusOneArray As Integer, startFrom As Integer, Optional TakeModIndexOfArray As Boolean = False)

        Dim reader As String = System.IO.File.ReadAllText(FileReturnPath())
        Dim E_table(IndexPlusOneArray - 1) As String
        Dim counter As Integer = 0
        For i = startFrom To reader.Length Step 7

            If Not E_table.Contains(JustConvertToD(Mid(reader, i, 7))) Then

                If TakeModIndexOfArray Then
                    'use for generate numbers(n), its value les than (n)
                    E_table(counter) = JustConvertToD(Mid(reader, i, 7)) Mod IndexPlusOneArray
                Else
                    E_table(counter) = JustConvertToD(Mid(reader, i, 7))
                End If

                counter += 1
                If counter = IndexPlusOneArray Then
                    Exit For
                End If
            End If

        Next

        'E_table is my Encryption Table
        Return E_table
    End Function

    Function INV_Order(table() As String, EncryptedBits As String)
        Dim inv_Table(127)
        For i = 0 To table.Length - 1
            inv_Table(table(i)) = i

        Next

        Dim EncryptedBits_char() As Char = EncryptedBits.ToCharArray
        Dim DecryptBits(127) As String
        Dim team
        For i = 0 To EncryptedBits.Length - 1
            team = EncryptedBits(i)
            DecryptBits(inv_Table(i)) = team

        Next


        Return Join(DecryptBits, "")
    End Function

    Function Ip_Procces_Encryption(E_table() As String, str As String)
        Dim E_Procces(127) As String
        For i = 1 To str.Length
            E_Procces(E_table(i - 1)) = Mid(str, i, 1)

        Next



        Return Join(E_Procces, "")
    End Function


    Function SplitString(TheString As String, StringLen As Integer) As String()
        Dim ArrCount As Integer  'as it is declared locally, it will automatically reset to 0 when this is called again
        Dim I As Long  'we are going to use it.. so declare it (with local scope to avoid breaking other code)
        Dim TempArray() As String
        ReDim TempArray((Len(TheString) - 1) \ StringLen)
        For I = 1 To Len(TheString) Step StringLen
            TempArray(ArrCount) = Mid$(TheString, I, StringLen)
            ArrCount = ArrCount + 1
        Next
        SplitString = TempArray   'actually return the value
    End Function

    Function Chaos_Table(InputKeyR As String, InputKeyX As String) As String()
        Dim x(1000) As Double
        Dim r As Double

        x(0) = ChaosAsc(InputKeyX) Mod 0.31
        r = ChaosAsc(InputKeyR) Mod 3.73
        If r = 0 Then
            r += 0.39
        ElseIf x(0) = 0 Then
            x(0) = 0.43
        End If
        For i = 0 To x.Length - 2

            If Not x.Contains(x(i) * r * (1 - x(i)) Mod 1) Then
                x(i + 1) = x(i) * r * (1 - x(i)) Mod 1
                r = r + 0.5 Mod 4
            End If
        Next
        Dim StrArray As String() = Array.ConvertAll(x, Function(y) y.ToString)
        Chaos_Table = StrArray

    End Function

    Function XorOperation(First As String, Second As String, IndexValue As Integer)


        Dim XorResult(IndexValue) As String
        For i = 0 To IndexValue - 1
            XorResult(i) = Convert.ToString(Convert.ToInt32(First(i)) Xor Convert.ToInt32(Second(i)))
        Next
        Return Join(XorResult, "")
    End Function

    Function MatrixFormShift(str As String, keyShit() As String, Optional decryptMode As Boolean = False)
        Dim ArrSplit = SplitString(str, 8)
        For i = 0 To 7
            If decryptMode Then
                ArrSplit(i) = shifting(ArrSplit(i), 8 - keyShit(i))
            Else
                ArrSplit(i) = shifting(ArrSplit(i), keyShit(i))
            End If
        Next
        Return Join(ArrSplit, "")
    End Function

    Function shifting(MyString As String, NumberOfShinftingLoop As Integer)
        NumberOfShinftingLoop = NumberOfShinftingLoop Mod MyString.Length
        Return (Mid(MyString, NumberOfShinftingLoop + 1, MyString.Length - NumberOfShinftingLoop) + Mid(MyString, 1, NumberOfShinftingLoop))
    End Function

    Function shift_Discard(MyString As String, NumberOfShinftingLoop As Integer, Optional DecryptMode As Boolean = False)
        Dim NewShifted As String
        Dim ValueThatShifted As String
        Dim ReplaceShifted = ""

        If Not DecryptMode Then
            For i = 1 To NumberOfShinftingLoop
                ReplaceShifted += "1"
            Next
            NewShifted = Mid(MyString, 1, MyString.Length - NumberOfShinftingLoop) & ReplaceShifted
            ValueThatShifted = Mid(MyString, MyString.Length - NumberOfShinftingLoop + 1, NumberOfShinftingLoop)
            discard.Add(ValueThatShifted)

        Else

            Dim RetriveShifted, PartWithoutShift As String

            Dim shiftStage = (discard.Count - 1) Mod 80
            RetriveShifted = discard.Item(shiftStage)
            discard.RemoveAt(shiftStage)
            PartWithoutShift = Mid(MyString, 1, MyString.Length - RetriveShifted.Length)
            NewShifted = PartWithoutShift & RetriveShifted

        End If

        Return NewShifted
    End Function

    Function FileReturnPath()
        Return "bits.txt"
    End Function


    Function SerpentMode(str As String, Optional DecryptMode As Boolean = False)
        Dim A, B, C, D As String
        A = Mid(str, 1, 16)
        B = Mid(str, 17, 16)
        C = Mid(str, 33, 16)
        D = Mid(str, 49, 16)

        If Not DecryptMode Then
            A = shift_Discard(A, 13)
            C = shift_Discard(C, 13)

            B = XorOperation(B, C, 16)
            B = XorOperation(B, A, 16)

            D = XorOperation(D, shifting(A, 3), 16)
            D = XorOperation(D, C, 16)

            B = shift_Discard(B, 1)
            D = shift_Discard(D, 1)

            A = XorOperation(A, B, 16)
            A = XorOperation(A, D, 16)

            A = shift_Discard(A, 5)

            C = XorOperation(C, D, 16)
            C = XorOperation(C, shifting(B, 7), 16)
        Else

            C = XorOperation(C, D, 16)

            C = XorOperation(C, shifting(B, 7), 16)

            A = shift_Discard(A, 5, True)

            A = XorOperation(A, B, 16)
            A = XorOperation(A, D, 16)

            D = shift_Discard(D, 1, True)
            B = shift_Discard(B, 1, True)

            D = XorOperation(D, shifting(A, 3), 16)
            D = XorOperation(D, C, 16)

            B = XorOperation(B, C, 16)
            B = XorOperation(B, A, 16)

            C = shift_Discard(C, 13, True)
            A = shift_Discard(A, 13, True)
        End If

        Return A + B + C + D
    End Function

    Function ChaosAsc(Key As String)
        Dim FullAsc As Integer = 0
        For i = 1 To Key.Length
            FullAsc += Asc(Mid(Key, i, 1))
        Next
        Return FullAsc
    End Function


    Private Sub Button2_Click_1(sender As Object, e As EventArgs) Handles Button2.Click
        Process.Start("Encryption.txt")
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Process.Start("Decryption.txt")
    End Sub
End Class
