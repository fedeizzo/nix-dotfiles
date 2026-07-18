use anyhow::Result;
use std::sync::Arc;

use crate::domain::finance::FinanceProvider;

pub struct FinanceService<P: FinanceProvider> {
    // We hold the Trait behind an Arc using Static Dispatch (Zero-Cost Abstraction!)
    provider: Arc<P>,
}

impl<P: FinanceProvider> Clone for FinanceService<P> {
    fn clone(&self) -> Self {
        Self {
            provider: Arc::clone(&self.provider),
        }
    }
}

impl<P: FinanceProvider> FinanceService<P> {
    pub fn new(provider: Arc<P>) -> Self {
        Self { provider }
    }

    #[tracing::instrument(skip(self), err)]
    pub async fn get_user(&self) -> Result<crate::domain::finance::User> {
        Ok(self.provider.get_user().await?)
    }

    #[tracing::instrument(skip(self), err)]
    pub async fn get_accounts(&self) -> Result<Vec<crate::domain::finance::Account>> {
        Ok(self.provider.get_accounts().await?)
    }

    #[tracing::instrument(skip(self), err)]
    pub async fn get_categories(&self) -> Result<Vec<crate::domain::finance::Category>> {
        Ok(self.provider.get_categories().await?)
    }

    #[tracing::instrument(skip(self), err)]
    pub async fn get_tags(&self) -> Result<Vec<crate::domain::finance::Tag>> {
        Ok(self.provider.get_tags().await?)
    }

    #[tracing::instrument(skip(self), err)]
    pub async fn get_unreviewed_transactions(&self) -> Result<Vec<crate::domain::finance::Transaction>> {
        Ok(self.provider.get_unreviewed_transactions().await?)
    }

    #[tracing::instrument(skip(self), err)]
    pub async fn calculate_net_worth(&self) -> Result<f64> {
        let accounts = self.provider.get_accounts().await?;
        let _currency = self.provider.get_user().await?.primary_currency;

        let net_worth = accounts.iter().map(|account| account.balance).sum();

        Ok(net_worth)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::domain::finance::Account;

    pub struct FakeFinanceProvider {}

    impl FinanceProvider for FakeFinanceProvider {
        async fn get_accounts(&self) -> Result<Vec<Account>, crate::domain::finance::Error> {
            Ok(vec![
                Account {
                    name: "Checking".into(),
                    institution_name: "Chase".into(),
                    account_type: "depository".into(),
                    subtype: "checking".into(),
                    balance: 1050.0,
                    currency: "USD".into(),
                    balance_as_of: chrono::Utc::now(),
                    status: crate::domain::finance::AccountStatus::Active,
                },
                Account {
                    name: "Savings".into(),
                    institution_name: "Chase".into(),
                    account_type: "depository".into(),
                    subtype: "savings".into(),
                    balance: 50.50,
                    currency: "USD".into(),
                    balance_as_of: chrono::Utc::now(),
                    status: crate::domain::finance::AccountStatus::Active,
                },
            ])
        }

        async fn get_user(
            &self,
        ) -> Result<crate::domain::finance::User, crate::domain::finance::Error> {
            Ok(crate::domain::finance::User {
                name: "Fake User".into(),
                budget_name: "Fake Budget".into(),
                api_key_label: "fake_label".into(),
                primary_currency: "USD".into(),
            })
        }

        async fn get_categories(
            &self,
        ) -> Result<Vec<crate::domain::finance::Category>, crate::domain::finance::Error> {
            Ok(vec![crate::domain::finance::Category {
                id: 1,
                name: "Food".into(),
                description: None,
                is_income: false,
                archived: false,
            }])
        }

        async fn get_tags(
            &self,
        ) -> Result<Vec<crate::domain::finance::Tag>, crate::domain::finance::Error> {
            Ok(vec![crate::domain::finance::Tag {
                id: 10,
                name: "Vacation".into(),
                description: None,
                archived: false,
            }])
        }

        async fn get_unreviewed_transactions(
            &self,
        ) -> Result<Vec<crate::domain::finance::Transaction>, crate::domain::finance::Error> {
            Ok(vec![
                crate::domain::finance::Transaction {
                    id: 1,
                    date: "2026-07-19".into(),
                    payee: "Fake Store".into(),
                    amount: 10.0,
                    currency: "USD".into(),
                    to_base: 10.0,
                    category_id: Some(2),
                    tag_ids: vec![1],
                    original_name: Some("Store INC".into()),
                    notes: None,
                    source: Some("plaid".into()),
                }
            ])
        }
    }

    #[tokio::test]
    async fn test_calculate_net_worth() {
        let fake = FakeFinanceProvider {};
        let service = FinanceService::new(std::sync::Arc::new(fake));

        let net_worth = service.calculate_net_worth().await.unwrap();
        // 1050.0 + 50.50 = 1100.50
        assert!((net_worth - 1100.50).abs() < f64::EPSILON);
    }

    #[tokio::test]
    async fn test_get_categories() {
        let fake = FakeFinanceProvider {};
        let service = FinanceService::new(std::sync::Arc::new(fake));

        let categories = service.get_categories().await.unwrap();
        assert_eq!(categories.len(), 1);
        assert_eq!(categories[0].name, "Food");
    }

    #[tokio::test]
    async fn test_get_tags() {
        let fake = FakeFinanceProvider {};
        let service = FinanceService::new(std::sync::Arc::new(fake));

        let tags = service.get_tags().await.unwrap();
        assert_eq!(tags.len(), 1);
        assert_eq!(tags[0].name, "Vacation");
    }
}
