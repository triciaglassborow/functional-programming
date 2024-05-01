﻿module Assignment
open System

module Task123 =
    // -------------------------------------------------------------------------------------- //
    //                                         TASK 1                                         //
    // -------------------------------------------------------------------------------------- //

    //defining a type named account with an accountNumber (string) and balance (float) field.
    type Account = 
        {
            Name: string 
            mutable Balance: float
        }

        //Print member
        member this.Print =
            Console.WriteLine($"Account Name: {this.Name} Account Balance: {this.Balance}")
        //update account balance method
        member this.UpdateBalance newAccountBalance =
            {this with Balance = newAccountBalance}
        
    //withdraw method
    let Withdraw (account :Account) = 
        Console.WriteLine()
        account.Print
        //user enter the amount to be withdrawn
        Console.Write("Enter amount to be withdrawn: ")
        let amount = float(Console.ReadLine())

        let newBalance = account.Balance - amount
    
        //checking that the user enters a positive number
        if (amount > 0) then
            //checking the account has the funds, before withdrawing
            if (newBalance < 0.0) then printfn "Not enough funds"
            else account.Balance <- newBalance
        else printfn "INVALID"
        

        Console.WriteLine("---------------- CLOSING DETAILS ---------------- ")
        account.Print

    //let Deposit 
    let Deposit (account :Account) = 
        Console.WriteLine()
        account.Print

        //user enter the amount to be deposited
        Console.Write("Enter amount to be deposited: ")
        let amount = float(Console.ReadLine())
        if (amount > 0) then
            //new balance
            account.Balance <- account.Balance + amount
        else printfn "INVALID"

        Console.WriteLine("---------------- CLOSING DETAILS ---------------- ")
        account.Print

    // -------------------------------------------------------------------------------------- //
    //                                         TASK 2                                         //
    // -------------------------------------------------------------------------------------- //

    //compairing the balance passed to see if the balance is high, okay or low
    let CheckAccount balance =
        match balance with
        | b when b > 100.00 -> printfn "Balance is high"
        | b when b >= 10.00 -> printfn "Balance is okay"
        | b when b < 10.00 -> printfn "Balance is low"
        | _ -> printfn "Invalid"
        

    // -------------------------------------------------------------------------------------- //
    //                                         RUN METHOD                                     //
    // -------------------------------------------------------------------------------------- //
    let run() =
        //creating the accounts
        let Account1 = {Name = "0001"; Balance = 0.0}
        let Account2 = {Name = "0002"; Balance = 51.0}
        let Account3 = {Name = "0003"; Balance = 5.0}
        let Account4 = {Name = "0004"; Balance = 100.0}
        let Account5 = {Name = "0005"; Balance = 110.0}
        let Account6 = {Name = "0006"; Balance = 10.0}

        // -------------------------------------------------------------------------------------- //
        //                                         TASK 3                                         //
        // -------------------------------------------------------------------------------------- //

        //defining a list of all accounts
        let accounts = [Account1; Account2; Account3; Account4; Account5; Account6]

        //List partition. going through accounts, if the zccount balance less then 50 it goes into
        //the first list. if its 50 or more it goes to the second list. these two lists are in a tuple
        let partitionAccounts = List.partition (fun i -> i.Balance < 50 ) accounts
        let lowAccountBalance = fst partitionAccounts
        let highAccountBalance = snd partitionAccounts

        Console.WriteLine ("---------------- LIST ALL ACCOUNTS ---------------")
        for i in accounts do 
            i.Print

        //task 1 output
        Console.WriteLine()
        Console.WriteLine ("-------------------- WITHDRAW --------------------")
        Console.Write("Which account would you like to withdraw from (1-6): ")
        let mutable withdrawAccSelect = int(Console.ReadLine())

        while withdrawAccSelect > 6 || withdrawAccSelect <= 0 do
            printfn"INVALID ACCOUNT NAME"
            Console.Write("Which account would you like to deposit to (1-6): ")
            withdrawAccSelect <- int(Console.ReadLine())

        let withdrawAccName = "000" + string(withdrawAccSelect)  

        match withdrawAccName with
        | x when x = Account1.Name -> Withdraw Account1
        | x when x = Account2.Name -> Withdraw Account2
        | x when x = Account3.Name -> Withdraw Account3
        | x when x = Account4.Name -> Withdraw Account4
        | x when x = Account5.Name -> Withdraw Account5
        | x when x = Account6.Name -> Withdraw Account6
        | _ -> printfn "Invalid"

        Console.WriteLine()
        Console.WriteLine ("-------------------- DEPOSIT --------------------")
        Console.Write("Which account would you like to deposit to (1-6): ")
        let mutable depositAccSelect = int(Console.ReadLine())
        while depositAccSelect > 6 || depositAccSelect <= 0 do
            printfn"INVALID ACCOUNT NAME"
            Console.Write("Which account would you like to deposit to (1-6): ")
            depositAccSelect <- int(Console.ReadLine())

        let depositAccName = "000" + string(depositAccSelect)  


        match depositAccName with
        | x when x = Account1.Name -> Deposit Account1
        | x when x = Account2.Name -> Deposit Account2
        | x when x = Account3.Name -> Deposit Account3
        | x when x = Account4.Name -> Deposit Account4
        | x when x = Account5.Name -> Deposit Account5
        | x when x = Account6.Name -> Deposit Account6
        | _ -> printfn "Invalid"
        
        //task 2 output
        Console.WriteLine()
        Console.WriteLine ("--- LIST ALL ACCOUNTS PLUS QUICK CHECK BALANCE ---")
        for i in accounts do 
            i.Print
            CheckAccount i.Balance

        //task 3 output
        Console.WriteLine()
        Console.WriteLine ("----------- ACCOUNTS WITH LESS THEN 50 -----------")
        for i in lowAccountBalance do
            i.Print

        Console.WriteLine ("----------- ACCOUNTS WITH 50 OR MORE -------------")
        for i in highAccountBalance do
            i.Print


// -------------------------------------------------------------------------------------- //
//                                         TASK 4                                         //
// -------------------------------------------------------------------------------------- //
module Task4 = 
    open System.Threading
    //defining record
    type Ticket = 
        {   
            //record fields
            seat: int 
            mutable customer: string
        }
    
        //print member
        member this.Print =
            Console.WriteLine($"Ticket: {this.seat} Customer: {this.customer}")
    
    //creating list of Tickets with a seat number and blank customer name
    let mutable tickets = [for n in 1..10 -> {Ticket.seat = n; Ticket.customer = ""}]
    //defining locking object
    let lockobj = new Object()

    //method for displaying the list of Tickets
    let DisplayTicket() =
        Console.WriteLine ("-------------------- Tickets --------------------")
        for i in tickets do
            i.Print

    //method for letting a customer book a seat
    let BookSeat() = 
        Console.WriteLine ("------------------- Book Seat --------------------")
        //asking for cutomers name
        Console.Write("Enter customer name: ")
        let customerName = Console.ReadLine()

        //asking what seat they want
        Console.Write("Enter the seat: ")
        //mutable so it can be updated to a valid seat if they don't enter one to begin with
        let mutable selectedSeat = int(Console.ReadLine())

        //while they input an invalid seat, they will continue to be asked for a seat
        while (selectedSeat <= 0 || selectedSeat > 10) do
            printfn "INVALID SEAT"
            Console.Write("Enter the seat: ")
            selectedSeat <- int(Console.ReadLine())
            
        //making the number of the seat selected line up with its postition in the list
        let selectedTicket =  tickets.[selectedSeat - 1]

        Console.WriteLine()
        
        //checking the Ticket has a blank customer name, so it can be booked
        if selectedTicket.customer = "" then
                    selectedTicket.customer <- customerName
                    printfn "                    SEAT BOOKED" 
                else
                    printfn "                 SEAT NOT AVAILABLE" 

        Console.WriteLine()
        

    //method that has the full sequence of viewing tickets and then booking a seat
    let TaskFunction thread = 
        //locking so one thread can go through the process of reading the list, selecting a ticket
        //and updating the list to have the customers name before the next thread starts. Stopping a
        //race condition from happening 
        lock lockobj (fun() -> 
            Console.WriteLine ("################### " + thread + " ###################")
        
            DisplayTicket()
            BookSeat()
    
            Console.WriteLine ("################# " + thread + " END #################")
        )
        Thread.Sleep(1000)
       
    // -------------------------------------------------------------------------------------- //
    //                                         RUN METHOD                                     //
    // -------------------------------------------------------------------------------------- //
    let run() =
        Console.WriteLine()
        //threads run at the same time
        let thread1 = new Thread(fun() -> TaskFunction "THREAD 1 ")
        thread1.Start()
        let thread2 = new Thread(fun() -> TaskFunction "THREAD 2 ")
        thread2.Start()

        //wait for both for finish
        thread1.Join()
        thread2.Join()

        DisplayTicket()

