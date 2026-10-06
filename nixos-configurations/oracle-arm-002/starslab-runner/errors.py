"""Trusted, fixed diagnostic reasons safe for local and display reports."""


class RunnerValidationError(ValueError):
    reason = 'execution_failed'
    message = 'Runner validation failed'

    def __init__(self):
        super().__init__(self.message)


class AccountIdentityMismatch(RunnerValidationError):
    reason = 'account_identity_mismatch'
    message = 'Verified account identity differs from local configuration'


class WalletCashMismatch(RunnerValidationError):
    reason = 'wallet_cash_below_journal'
    message = 'Exchange cash is below journal cash'


class WalletHoldingsMismatch(RunnerValidationError):
    reason = 'wallet_holdings_mismatch'
    message = 'Exchange holdings differ from the local journal'


class InvalidWalletData(RunnerValidationError):
    reason = 'invalid_wallet_data'
    message = 'Exchange wallet data is invalid'


class InvalidFeeQuote(RunnerValidationError):
    reason = 'fee_quote_unavailable_or_excessive'
    message = 'Authenticated fee is unavailable or above the safety ceiling'
