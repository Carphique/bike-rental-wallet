package bikerental

sealed trait WalletEvent
case class Deposit(amount: BigDecimal) extends WalletEvent
case class Ride(durationMinutes: Int, cost: BigDecimal) extends WalletEvent
case class PromoBonus(amount: BigDecimal) extends WalletEvent
case class Compensation(amount: BigDecimal) extends WalletEvent

// 2. Стан системи (State)
case class WalletState(
  balance: BigDecimal = BigDecimal(0),
  totalRideTime: Int = 0,
  rideHistory: List[Ride] = List.empty
) {
  // Аналітичний показник: Топ-3 найдовших поїздок
  def top3LongestRides: List[Ride] = 
    rideHistory.sortBy(_.durationMinutes)(Ordering[Int].reverse).take(3)
}

// 3. Бізнес-логіка (обробка подій та розрахунки)
object WalletService {
  
  // Функція переходу: бере поточний стан і подію, повертає або помилку (String), або новий стан
  def applyEvent(state: WalletState, event: WalletEvent): Either[String, WalletState] = event match {
    case Deposit(amount) => 
      Right(state.copy(balance = state.balance + amount))
      
    case r @ Ride(duration, cost) =>
      if (state.balance >= cost) {
        Right(state.copy(
          balance = state.balance - cost,
          totalRideTime = state.totalRideTime + duration,
          rideHistory = r :: state.rideHistory // додаємо поїздку в історію
        ))
      } else {
        Left("Овердрафт: недостатньо коштів на балансі для поїздки")
      }
      
    case PromoBonus(amount) =>
      Right(state.copy(balance = state.balance + amount))
      
    case Compensation(amount) =>
      Right(state.copy(balance = state.balance + amount))
  }

  // Допоміжна функція для "програвання" історії подій з нуля
  def processEvents(events: List[WalletEvent]): Either[String, WalletState] = {
    events.foldLeft[Either[String, WalletState]](Right(WalletState())) { (stateEither, event) =>
      stateEither.flatMap(state => applyEvent(state, event))
    }
  }
}