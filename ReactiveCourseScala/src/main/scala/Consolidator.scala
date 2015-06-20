import Publisher._
/**
 * Created by mingchen on 4/20/15.
 */
class Consolidator(observed: List[BankAccount]) extends Subscriber{
  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute

  def compute =  {
    total = observed.map(_.currentBalance).sum
  }

  def handler(pub: Publisher) = compute

  def totalBalacne = total
}

