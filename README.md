# 🏠 Real Estate Tokenization Smart Contract

A Clarity smart contract that enables **fractional ownership** of real estate properties through tokenization on the Stacks blockchain! 🚀

## 🌟 Features

- 🏢 **Create Properties**: Property owners can tokenize their real estate
- 💰 **Buy Tokens**: Users can purchase fractional ownership tokens
- 🔄 **Transfer Tokens**: Seamless token transfers between users
- 💎 **Dividend Distribution**: Property owners can distribute profits to token holders
- 📊 **Ownership Tracking**: View ownership percentages and token balances
- 🔒 **Secure Transactions**: Built-in validation and error handling

## 🛠️ Core Functions

### Property Management
- `create-property` - Tokenize a new real estate property
- `deactivate-property` - Disable token sales for a property
- `get-property` - View property details

### Token Operations
- `buy-tokens` - Purchase fractional ownership tokens
- `transfer-tokens` - Transfer tokens to another user
- `get-user-tokens` - Check token balance

### Dividend System
- `distribute-dividends` - Property owners distribute profits
- `claim-dividend` - Token holders claim their share

### Analytics
- `get-property-ownership-percentage` - Calculate ownership percentage
- `calculate-token-value` - Get current token value

## 🚀 Getting Started

### Deploy the Contract

```bash
clarinet deploy
```

### Create Your First Property

```bash
clarinet console
```

```clarity
(contract-call? .real-estate-tokenization create-property "Luxury Apartment NYC" u1000000 u1000)
```

### Buy Tokens

```clarity
(contract-call? .real-estate-tokenization buy-tokens u1 u100)
```

### Check Your Ownership

```clarity
(contract-call? .real-estate-tokenization get-property-ownership-percentage u1 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM)
```

## 📋 Usage Examples

### 1. Property Owner Workflow
1. Create a tokenized property with total value and token supply
2. Users buy tokens with STX
3. Distribute dividends to token holders periodically
4. Manage property status (active/inactive)

### 2. Investor Workflow
1. Browse available properties
2. Purchase tokens for fractional ownership
3. Transfer tokens to other users
4. Claim dividends based on ownership percentage

## 🔧 Contract Architecture

- **Properties Map**: Stores property details and tokenization info
- **Property Tokens Map**: Tracks token ownership per user per property
- **User Properties Map**: Quick lookup for user's property investments
- **Dividend System**: Proportional profit distribution

## 💡 Key Concepts

- **Fractional Ownership**: Own a percentage of high-value real estate
- **Tokenization**: Real estate value divided into tradeable tokens
- **Liquidity**: Easy transfer of ownership stakes
- **Transparency**: All transactions recorded on blockchain

## 🛡️ Security Features

- Owner-only property creation
- Balance validation before transfers
- Active property checks
- Input validation and error handling

## 📈 Benefits

- 🌍 **Accessibility**: Invest in real estate with smaller amounts
- 💧 **Liquidity**: Trade property tokens easily
- 🔍 **Transparency**: All ownership records on-chain
- 📊 **Diversification**: Own fractions of multiple properties
- 🤝 **Democratic**: Proportional dividend distribution

Start building your real estate investment portfolio today! 🏡✨
```

**Git Commit Message:**
```
feat: implement real estate tokenization MVP with fractional ownership
```

**GitHub Pull Request Title:**
```
🏠 Add Real Estate Tokenization Smart Contract - Fractional Ownership MVP
```

**GitHub Pull Request Description:**
```
## 🏠 Real Estate Tokenization MVP

This PR introduces a complete real estate tokenization smart contract that enables fractional ownership of properties on the Stacks blockchain.

### ✨ What's Added

- **Property Tokenization**: Create tokenized real estate properties with configurable token supply
- **Fractional Ownership**: Users can buy/sell tokens representing property ownership percentages  
- **Token Transfers**: Seamless P2P token transfers between users
- **Dividend Distribution**: Property owners can distribute profits proportionally to token holders
- **Ownership Analytics**: Calculate ownership percentages and token values
- **Security Controls**: Owner permissions, balance validations, and comprehensive error handling

### 🛠️ Technical Implementation

- 150+ lines of clean Clarity code
- Comprehensive data structures for properties, tokens, and user mappings
- Read-only functions for analytics and data retrieval
- Public functions for all core operations
- Built-in error handling with descriptive error codes

### 📚 Documentation

- Complete README with usage examples and feature descriptions
- Clear function documentation and workflow guides
- Getting started instructions for developers

This MVP demonstrates core fractional ownership concepts and provides a solid foundation for real estate tokenization on Stacks.



