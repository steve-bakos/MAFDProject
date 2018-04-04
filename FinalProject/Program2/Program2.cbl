       identification division.
       program-id. Program2.
       author. Sarah Powell
       date-written. 31 March 2018.
      * Description: Final Project Program 2 - Data Split and Count

       environment division.
       configuration section.
       input-output section.
       
       file-control.

      * Input-file declaration
           select input-file
               assign to "../../../../Program1/data/valid.out"
               organization is line sequential.

      * Output-file declarations

           select snl-file
               assign to "../../../data/sales-layaway-records.out"
               organization is line sequential.

           select ret-file
               assign to "../../../data/returns-records.out"
               organization is line sequential.

           select tot-file
               assign to "../../../data/counts-controls-totals.out"
               organization is line sequential.


       data division.
       file section.

      * Valid data line
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 valid-in.
           05 vi-trans-code                    pic x.
           05 vi-trans-amt                     pic 9(5)v99.
           05 vi-pay-type                      pic xx.
           05 vi-store-num                     pic 99.
           05 vi-invoice-number.
               10 vi-inv-prefix                pic xx.
               10 vi-inv-hyphen                pic x.
               10 vi-inv-number                pic 9(6).
           05 vi-sku-code                      pic x(15).

      * Sales and Layaway Records out file
       fd snl-file
           data record is snl-line
           record contains 36 characters.

       01  snl-line                            pic x(36).

      * Returns out file
       fd  ret-file
           data record is rec-line
           record contains 36 characters.

       01  rec-line                            pic x(36).

      * Returns out file
       fd  tot-file
           data record is tot-line
           record contains 70 characters.

       01  tot-line                            pic x(70).

       
       working-storage section.

       01  ws-sales-layaway-line.
           05 ws-trans-code                    pic x.
           05 ws-trans-amt                     pic 9(5)v99.
           05 ws-pay-type                      pic xx.
           05 ws-store-num                     pic 99.
           05 ws-invoice-number.
               10 ws-inv-prefix                pic xx.
               10 ws-inv-hyphen                pic x.
               10 ws-inv-number                pic 9(6).
           05 ws-sku-code                      pic x(15).

       01  ws-returns-line.
           05 ws-trans-code                    pic x.
           05 ws-trans-amt                     pic 9(5)v99.
           05 ws-pay-type                      pic xx.
           05 ws-store-num                     pic 99.
           05 ws-invoice-number.
               10 ws-inv-prefix                pic xx.
               10 ws-inv-hyphen                pic x.
               10 ws-inv-number                pic 9(6).
           05 ws-sku-code                      pic x(15).
    
      * Sales and Layaway totals report lines
       01  ws-sl-header.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(24)
               value " SALES & LAYAWAY TOTALS ".
           05  filler                          pic x(23)
               value "***********************".

       01 ws-sl-transactions-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(34)
               value "TOTAL SALE & LAYAWAY TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-snl-trans-out                pic zz9
               value 0. 

       01 ws-sl-amount-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(27)
               value "TOTAL SALE & LAYWAY AMOUNT:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-sales-lay-out                pic $$$,$$$,$$9.99
               value 0. 

       01 ws-sl-cash-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(39)
               value "TOTAL SALE & LAYAWAY CASH TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-cash-out                     pic $$$,$$$,$$9.99
               value 0. 

       01 ws-sl-debit-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(40)
               value "TOTAL SALE & LAYAWAY DEBIT TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-debit-out                    pic $$$,$$$,$$9.99
               value 0.

       01 ws-sl-credit-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(41)
               value "TOTAL SALE & LAYAWAY CREDIT TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-credit-out                   pic $$$,$$$,$$9.99
               value 0.

      * Sales totals report lines

       01  ws-sales-header.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(7)
               value " SALES ".
           05  filler                          pic x(40)
               value "****************************************".

       01 ws-sales-transactions-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(24)
               value "TOTAL SALE TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-num-sales-trans-out          pic zz9
               value 0. 

       01 ws-sale-amount-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(18)
               value "TOTAL SALE AMOUNT:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-total-sales-out              pic $$$,$$$,$$9.99
               value 0.

      * Layaway totals report lines

       01  ws-layaway-header.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(9)
               value " LAYAWAY ".
           05  filler                          pic x(38)
               value "**************************************".

       01 ws-layaway-transactions-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(27)
               value "TOTAL LAYAWAY TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-num-lay-trans-out            pic zz9
               value 0. 

       01 ws-layaway-amount-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(21)
               value "TOTAL LAYAWAY AMOUNT:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-total-layaway-out            pic $$$,$$$,$$9.99
               value 0.

      * Returns totals report lines

       01  ws-returns-header.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(9)
               value " RETURNS ".
           05  filler                          pic x(38)
               value "**************************************".

       01 ws-return-transactions-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(26)
               value "TOTAL RETURN TRANSACTIONS:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-num-ret-trans-out            pic zz9
               value 0. 

       01 ws-return-amount-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(20)
               value "TOTAL RETURN AMOUNT:".
           05  filler                          pic x(2)
               value spaces.
           05  ws-total-returns-out            pic $$$,$$$,$$9.99
               value 0.

      * Sales and Layaway Transactions by Store
       01 ws-sl-store-header-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(26)
               value " SALES & LAYAWAY BY STORE ".
           05  filler                          pic x(21)
               value "*********************".

       01  ws-sl-store-transactions.
           05  filler                          pic x
               value space.
           05  filler                          pic x(39)
               value "TOTAL SALES & LAYAWAY AMOUNT FOR STORE ".
           05  ws-store-number1                 pic xx
               value spaces.
           05  filler                          pic x
               value ":".
           05  filler                          pic x(2)
               value spaces. 
           05  ws-snl-amount-by-store          pic $$$,$$9.99
               value 0.

      * Returns Transactions by Store
       01 ws-returns-store-header-line.
           05  filler                          pic x
               value space.
           05  filler                          pic x(13)
               value "*************".
           05  filler                          pic x(18)
               value " RETURNS BY STORE ".
           05  filler                          pic x(30)
               value "******************************".

       01  ws-returns-store-transactions.
           05  filler                          pic x
               value space.
           05  filler                          pic x(31)
               value "TOTAL RETURNS AMOUNT FOR STORE ".
           05  ws-store-number2                 pic xx
               value spaces.
           05  filler                          pic x
               value ":".
           05  filler                          pic x(2)
               value spaces. 
           05  ws-ret-amount-by-store           pic $$$,$$9.99
               value 0.

       01  ws-sales-flag                       pic x
               value "N".
       01  ws-layaway-flag                     pic x
               value "N".
       01  ws-returns-flag                     pic x
               value "N".
       01  ws-sales-lay-flag                   pic x
               value "N".
       01  ws-loop1                            pic 9
               value 1.
       01  ws-loop2                            pic 9
               value 1.
       01  ws-num-sales-trans                  pic 999
               value 0.
       01  ws-num-lay-trans                    pic 999
               value 0.
       01  ws-num-snl-trans                    pic 999
               value 0.
       01  ws-num-return-trans                 pic 999
               value 0.
       01  ws-sales-lay-total                  pic 9(9)v9(3)
               value 0.
       01  ws-sales-total                      pic 9(9)v9(3)
               value 0.
       01  ws-layaway-total                    pic 9(9)v9(3)
               value 0.
       01  ws-returns-total                    pic 9(9)v9(3)
               value 0.

      * Playing with arrays
       01  ws-store-tbl.
           05  ws-store-sl-tot                 pic 9(9)v9(3)
                   occurs 7
                   value 0.
           05  ws-store-ret-tot                pic 9(9)v9(3)
                   occurs 7
                   value 0.
           05  ws-cash-tot                     pic 9(9)v9(3)
                   value 0.
           05  ws-debit-tot                    pic 9(9)v9(3)
                   value 0.
           05  ws-credit-tot                   pic 9(9)v9(3)
                   value 0.
       
       77  ws-eof                              pic x
               value "N".
       77  ws-sales-code                       pic x
               value "S".
       77  ws-layaway-code                     pic x
               value "L".
       77  ws-returns-code                     pic x
               value "R".
       77  ws-cash-code                        pic xx
               value "CA".
       77  ws-debit-code                       pic xx
               value "DB".
       77  ws-credit-code                      pic xx
               value "CR".
       77  ws-store1-code                      pic xx
               value "01".
       77  ws-store2-code                      pic xx
               value "02".
       77  ws-store3-code                      pic xx
               value "03".
       77  ws-store4-code                      pic xx
               value "07".
       77  ws-loop-limit                       pic 9
               value 5.
       
       procedure division.

       000-main.

           perform 100-openfiles.
           perform 200-readfiles.
           perform 300-writeline
               until ws-eof is equal to "Y".
           perform 400-writereport.
           perform 500-closefiles.

           accept return-code.
           goback.

       100-openfiles.
      * Open files
           open input input-file.
           open output snl-file ret-file tot-file.

       200-readfiles.
      * Read the record line
           read input-file
               at end move "Y" to ws-eof.

       300-writeline.
      * Reset flags
           move "N" to ws-sales-flag, ws-layaway-flag, ws-sales-lay-flag, ws-returns-flag.

      * Set flags and add transaction amount to correct total
           if vi-trans-code is equal to ws-sales-code
               move "Y" to ws-sales-flag
               add 1 to ws-num-sales-trans
               add vi-trans-amt to ws-sales-total
           end-if.

           if vi-trans-code is equal to ws-layaway-code
               move "Y" to ws-layaway-flag
               add 1 to ws-num-lay-trans
               add vi-trans-amt to ws-layaway-total
           end-if.

           if ws-sales-flag = "Y"  or ws-layaway-flag = "Y"
               add 1 to ws-num-snl-trans
               move "Y" to ws-sales-lay-flag
               add vi-trans-amt to ws-sales-lay-total

               if vi-pay-type = ws-cash-code then
                   
                   add vi-trans-amt to ws-cash-tot
               else    
                   if vi-pay-type = ws-debit-code then
                       add vi-trans-amt to ws-debit-tot
                   else
                       if vi-pay-type = ws-credit-code
                           add vi-trans-amt to ws-credit-tot
                       end-if
                   end-if
               end-if
           end-if.

           if vi-trans-code is equal to ws-returns-code
               add 1 to ws-num-return-trans
               move "Y" to ws-returns-flag
               add vi-trans-amt to ws-returns-total
           end-if.
      * Use flags for processing
           
           if ws-sales-lay-flag is equal to "Y" then
               write snl-line from valid-in
               if vi-store-num = ws-store1-code then
                   add vi-trans-amt to ws-store-sl-tot(1)
               else
                   if vi-store-num = ws-store2-code then
                       add vi-trans-amt to ws-store-sl-tot(2)
                   else
                       if vi-store-num = ws-store3-code then
                           add vi-trans-amt to ws-store-sl-tot(3)
                       else
                           if vi-store-num = ws-store4-code then
                               add vi-trans-amt to ws-store-sl-tot(4)
                           end-if
                       end-if
                   end-if
               end-if
           else
               if vi-trans-code = ws-returns-code then
                   write rec-line from valid-in
                   if vi-store-num = ws-store1-code then
                       add vi-trans-amt to ws-store-ret-tot(1)
                   else
                       if vi-store-num = ws-store2-code then
                           add vi-trans-amt to ws-store-ret-tot(2)
                       else
                           if vi-store-num = ws-store3-code then
                               add vi-trans-amt to ws-store-ret-tot(3)
                           else
                               if vi-store-num = ws-store4-code then
                                   add vi-trans-amt 
                                       to ws-store-ret-tot(4)
                               end-if
                           end-if
                       end-if
                   end-if
               end-if
           end-if.

      * Read next record
           read input-file at end move "Y" to ws-eof.

       400-writereport.
           perform 410-begintotals.
           perform 420-storesnlheader.
           perform 430-storesnltotals
               varying ws-loop1 from 1 by 1
               until ws-loop1 = ws-loop-limit.
           perform 440-storeretheader.
           perform 450-storerettotals
               varying ws-loop2 from 1 by 1
               until ws-loop2 = ws-loop-limit.

       410-begintotals.
           compute ws-snl-trans-out rounded = ws-num-snl-trans.
           compute ws-sales-lay-out rounded = ws-sales-lay-total.

           compute ws-cash-out rounded = ws-cash-tot
           compute ws-debit-out rounded = ws-debit-tot
           compute ws-credit-out rounded = ws-credit-tot

           compute ws-num-sales-trans-out rounded = ws-num-sales-trans.
           compute ws-total-sales-out rounded = ws-sales-total.

           compute ws-num-lay-trans-out rounded = ws-num-lay-trans.
           compute ws-total-layaway-out rounded = ws-layaway-total.

           compute ws-num-ret-trans-out rounded = ws-num-return-trans.
           compute ws-total-returns-out rounded = ws-returns-total.

           write tot-line from ws-sl-header.
           write tot-line from ws-sl-transactions-line
               after advancing 1 line.
           write tot-line from ws-sl-amount-line
               after advancing 2 lines.
           write tot-line from ws-sl-cash-line
               after advancing 2 lines.
           write tot-line from ws-sl-debit-line.
           write tot-line from ws-sl-credit-line.

           write tot-line from ws-sales-header
               after advancing 1 line.
           write tot-line from ws-sales-transactions-line
               after advancing 2 lines.
           write tot-line from ws-sale-amount-line
               after advancing 2 lines.

           write tot-line from ws-layaway-header
               after advancing 2 lines.
           write tot-line from ws-layaway-transactions-line
               after advancing 2 lines.
           write tot-line from ws-layaway-amount-line
               after advancing 2 lines.

           write tot-line from ws-returns-header
               after advancing 2 lines.
           write tot-line from ws-return-transactions-line
               after advancing 2 lines.
           write tot-line from ws-return-amount-line
               after advancing 2 lines.

       420-storesnlheader.
           write tot-line from ws-sl-store-header-line
               after advancing 2 lines.
           
       430-storesnltotals.

           move ws-loop1 to ws-store-number1, ws-store-number2
           compute ws-snl-amount-by-store rounded 
             = ws-store-sl-tot(ws-loop1)
           write tot-line from ws-sl-store-transactions
               after advancing 2 lines.
.

       440-storeretheader.
           write tot-line from ws-returns-store-header-line
               after advancing 2 lines.

       450-storerettotals.
           move ws-loop2 to ws-store-number1, ws-store-number2
           compute ws-ret-amount-by-store rounded 
             = ws-store-ret-tot(ws-loop2)
           write tot-line from ws-returns-store-transactions
               after advancing 2 lines.


       500-closefiles.

           close input-file.
           close snl-file ret-file tot-file.

       end program Program2.
