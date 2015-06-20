val account1 = new BankAccount
val account2 = new BankAccount
val obsever = new Consolidator(List(account1, account2))
obsever.totalBalacne

account1 deposit 50
obsever.totalBalacne

account1 withdraw 20
obsever.totalBalacne

account2 deposit 20
obsever.totalBalacne

account2 withdraw 15
obsever.totalBalacne


