/**
 * Created by mingchen on 4/19/15.
 */
import Publisher._

class BankAccount extends Publisher {
  private var balance = 0

  def currentBalance: Int = balance

  def deposit(amount: Int):Unit = {
    if (amount > 0) balance = balance + amount
    publish
  }

  def withdraw(amount: Int):Int = {
    if (amount > 0 && balance >= amount) {
      balance = balance - amount
      publish
      balance
    } else {
      throw new Error("insufficient fund")
    }
  }
}
