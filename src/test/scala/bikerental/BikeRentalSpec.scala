package bikerental

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BikeRentalSpec extends AnyFlatSpec with Matchers {
  
  "WalletService" should "коректно обробляти послідовність подій та рахувати аналітику" in {
    // Сценарій: старт -> поповнення -> промо -> поїздки -> компенсація
    val events = List(
      Deposit(100.0),        // Баланс: 100
      PromoBonus(50.0),      // Баланс: 150
      Ride(30, 40.0),        // Баланс: 110, Час: 30
      Ride(15, 20.0),        // Баланс: 90,  Час: 45
      Ride(45, 60.0),        // Баланс: 30,  Час: 90
      Ride(10, 15.0),        // Баланс: 15,  Час: 100
      Compensation(20.0)     // Баланс: 35
    )

    val result = WalletService.processEvents(events)
    
    // Перевіряємо, що помилок не було
    result.isRight shouldBe true
    
    val finalState = result.getOrElse(WalletState())
    
    // Перевіряємо баланс та загальний час
    finalState.balance shouldBe BigDecimal(35.0)
    finalState.totalRideTime shouldBe 100
    
    // Перевіряємо Топ-3 найдовших поїздок (мають бути 45, 30 та 15 хвилин)
    val top3 = finalState.top3LongestRides
    top3.length shouldBe 3
    top3.map(_.durationMinutes) shouldBe List(45, 30, 15)
  }

  it should "забороняти поїздку, якщо балансу недостатньо (захист від овердрафту)" in {
    val state = WalletState(balance = BigDecimal(10.0))
    
    // Спроба здійснити поїздку за 40.0
    val result = WalletService.applyEvent(state, Ride(30, 40.0))
    
    // Має повернутися помилка (Left)
    result.isLeft shouldBe true
    result.left.getOrElse("") shouldBe "Овердрафт: недостатньо коштів на балансі для поїздки"
  }
}