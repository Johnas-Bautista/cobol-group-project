# COBOL ATM Banking System

A fully-featured console-based ATM (Automated Teller Machine) banking system written in COBOL with an interactive terminal interface, complete with PIN authentication, file handling, and transaction receipts.

![Main Menu](screenshots/main-menu.png)

## 📋 Table of Contents
- [Overview](#overview)
- [Features](#features)
- [Screenshots](#screenshots)
- [Technical Implementation](#technical-implementation)
- [Requirements](#requirements)
- [Installation](#installation)
- [Usage Guide](#usage-guide)
- [Security Features](#security-features)
- [File Structure](#file-structure)
- [Error Handling](#error-handling)
- [Contributing](#contributing)

## Overview

**AEDESYL ATM** is a comprehensive console-based banking system that simulates real ATM operations. Built entirely in COBOL, it demonstrates professional file handling, security authentication, and transaction processing in a legacy programming environment.

### Key Highlights
- 🔐 Secure PIN-based authentication with attempt limits
- 💰 Complete banking operations (Deposit, Withdraw, Transfer)
- 🧾 Digital receipt generation with timestamps
- 📁 Persistent data storage using indexed files
- ⚠️ Robust error handling and validation
- 🎨 Formatted console interface with borders

## Features

### 🔑 User Authentication
- **Sign In**: Secure login with account number and 4-digit PIN
- **Sign Up**: New account creation with auto-generated account numbers
- **Security Lockout**: Automatic exit after 3 failed authentication attempts
- **PIN Verification**: Required for every transaction to ensure security

### 💳 Banking Operations

| Operation | Description |
|-----------|-------------|
| **Deposit** | Add funds to your account with confirmation |
| **Withdraw** | Remove funds with balance validation |
| **Check Balance** | View account details with receipt or screen display options |
| **Transfer** | Send money to other accounts with recipient validation |
| **Update Account** | Modify PIN, username, and personal information |
| **Delete Account** | Permanently remove account from the system |
| **Exit** | Secure logout with confirmation prompt |

### 🧾 Receipt System
- **Digital Receipts**: View formatted receipt layout on screen
- **Timestamps**: Every receipt includes date and time of transaction
- **Transaction Details**: Shows previous balance, transaction amount, and new balance
- **Optional Printing**: Choose between screen display or receipt format

### ⚡ Error Handling
- **Insufficient Balance**: Prevents withdrawals exceeding current balance
- **Invalid Account**: Alerts when account number doesn't exist
- **PIN Validation**: Ensures secure authentication for all operations
- **Input Validation**: Checks for valid amounts and account numbers

## Screenshots

### 1. Landing Page (Sign In/Sign Up)
![Landing Page](screenshots/landing-page.png)
*Main menu offering Sign In, Sign Up, and Exit options*

### 2. Sign Up Process
![Sign Up](screenshots/sign-up.png)
*New account creation with PIN setup and initial deposit*

### 3. Sign In Authentication
![Sign In](screenshots/sign-in.png)
*Account number and PIN entry for existing users*

### 4. Authentication Failure (3 Attempts)
![Auth Failure](screenshots/auth-failure.png)
*Security lockout message after exceeding maximum attempts*

### 5. Main Banking Menu
![Banking Menu](screenshots/banking-menu.png)
*Complete menu of available banking operations after successful login*

### 6. Deposit Transaction
![Deposit](screenshots/deposit.png)
*Deposit process with amount entry and PIN verification*

### 7. Withdraw Transaction
![Withdraw](screenshots/withdraw.png)
*Withdrawal with balance validation and confirmation*

### 8. Insufficient Balance Error
![Insufficient Balance](screenshots/insufficient-balance.png)
*Error message preventing overdraft when balance is too low*

### 9. Check Balance
![Check Balance](screenshots/check-balance.png)
*Account balance display with customer information*

### 10. Transfer Transaction
![Transfer](screenshots/transfer.png)
*Money transfer to another account with recipient validation*

### 11. Digital Receipt
![Receipt](screenshots/receipt.png)
*Formatted receipt showing transaction details with date and time*

### 12. Update Account
![Update Account](screenshots/update-account.png)
*Modify account information including PIN and personal details*

### 13. Delete Account
![Delete Account](screenshots/delete-account.png)
*Account deletion with confirmation prompt*

### 14. Exit Confirmation
![Exit](screenshots/exit-confirmation.png)
*Secure logout with option to continue or exit*

## Technical Implementation

### File Handling
The system uses **indexed sequential file organization** for efficient data management:

```cobol
SELECT INFO
    ASSIGN TO "C:\OPENCOBOL\TEXTFILE\ATM2.txt"
    ORGANIZATION IS INDEXED
    ACCESS MODE IS RANDOM
    RECORD KEY IS ACC-NUM
```

### Data Structure

| Field | Type | Size | Description |
|-------|------|------|-------------|
| `ACC-NUM` | Numeric | 5 digits | Account number (Primary Key) |
| `USER_NAME` | Alphanumeric | 9 characters | Customer name |
| `AGE` | Numeric | 2 digits | Customer age |
| `BALANCE` | Numeric (Signed) | 7 digits | Account balance |
| `PASSWORD` | Numeric | 4 digits | PIN for authentication |

### Authentication Flow
1. User enters account number
2. System validates account existence
3. User enters PIN (3 attempts maximum)
4. PIN verified against stored record
5. System grants access or locks out after failures

### Transaction Security
Every transaction requires PIN re-authentication:
- Prevents unauthorized operations
- Validates user identity before critical actions
- Maintains security throughout session

## Requirements

- **COBOL Compiler**: OpenCOBOL/GnuCOBOL 1.1 or higher
- **Operating System**: Windows, Linux, or macOS
- **Terminal Requirements**:
  - Minimum 120 columns × 30 rows
  - Support for COBOL screen positioning
  - Color display capability (optional but recommended)

## Installation

### 1. Install COBOL Compiler

**Windows:**
```bash
# Download GnuCOBOL from:
# https://sourceforge.net/projects/gnucobol/
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt-get update
sudo apt-get install open-cobol
```

**macOS:**
```bash
brew install gnu-cobol
```

### 2. Clone Repository
```bash
git clone https://github.com/Johnas-Bautista/cobol-group-project.git
cd cobol-group-project
```

### 3. Create Data Directory
```bash
# Windows
mkdir C:\OPENCOBOL\TEXTFILE\

# Linux/macOS (update path in atm.cbl line 12)
mkdir -p ~/cobol-data/
```

### 4. Compile Program
```bash
cobc -x atm.cbl -o atm
```

### 5. Run Application
```bash
# Windows
atm.exe

# Linux/macOS
./atm
```

## Usage Guide

### Creating Your First Account

1. **Launch the program** and select option `2` (Sign Up)
2. **Enter a PIN**: Must be 4 digits (1000-9999)
3. **Provide username**: Maximum 9 characters
4. **Enter your age**: 2-digit number
5. **Make initial deposit**: Any amount
6. **Save your account number**: Displayed after successful registration

### Signing In

1. Select option `1` (Sign In) from the main menu
2. Enter your 6-digit **account number**
3. Enter your 4-digit **PIN**
4. ⚠️ **Important**: You have only 3 attempts before automatic lockout

### Making Transactions

#### Deposit Money
```
1. Select option 1 (Deposit) from banking menu
2. Enter amount to deposit (or 0 to cancel)
3. Confirm transaction
4. Enter PIN for verification
5. View new balance
6. Optional: Print digital receipt
```

#### Withdraw Money
```
1. Select option 2 (Withdraw) from banking menu
2. Enter amount to withdraw (or 0 to cancel)
3. System validates sufficient balance
4. Confirm transaction
5. Enter PIN for verification
6. View new balance
7. Optional: Print digital receipt
```

#### Check Balance
```
1. Select option 3 (Check Balance)
2. Choose display format:
   - SHOW: View on screen
   - PRINT: Digital receipt format
3. View complete account information
```

#### Transfer Money
```
1. Select option 4 (Transfer) from banking menu
2. Enter recipient account number
3. System validates recipient exists
4. Enter transfer amount
5. Confirm transaction
6. Enter PIN for verification
7. System validates sufficient balance
8. Transfer processed and receipt available
```

#### Update Account
```
1. Select option 5 (Update Account)
2. Enter account number to update
3. Provide new PIN
4. Update username
5. Update age
6. Changes saved immediately
```

#### Delete Account
```
1. Select option 6 (Delete Account)
2. Enter account number to delete
3. Confirm deletion (irreversible)
4. Account permanently removed from system
```

### Viewing Receipts

After each transaction, you'll be prompted:
```
DO YOU WANT A RECEIPT?(Y/N):
```

**Digital Receipt includes:**
- 🏦 Bank name (AEDESYL BANK)
- 📅 Transaction date (YYYY/MM/DD format)
- 🕐 Transaction time (HH:MM AM/PM format)
- 🆔 Account number
- 👤 Customer name
- 💵 Previous balance
- 💰 Transaction amount
- ✅ New balance

## Security Features

### 🔐 Multi-Layer Protection

1. **PIN Authentication**
   - Required at login and every transaction
   - 4-digit numeric PIN
   - Stored securely in indexed file

2. **Attempt Limiting**
   - Maximum 3 failed login attempts
   - Automatic system lockout
   - Prevents brute-force attacks

3. **Transaction Verification**
   - PIN re-verification for critical operations
   - Confirmation prompts for irreversible actions
   - Account validation before transfers

4. **Balance Protection**
   - Prevents overdrafts
   - Validates sufficient funds before withdrawal/transfer
   - Clear error messages for insufficient balance

5. **Account Validation**
   - Verifies account existence
   - Checks recipient accounts during transfers
   - Prevents operations on non-existent accounts

## File Structure

```
cobol-group-project/
├── atm.cbl                 # Main COBOL source code (1000+ lines)
├── ATM2.txt                # Indexed data file (auto-generated)
├── README.md               # This documentation
└── screenshots/            # Program screenshots (create this folder)
    ├── landing-page.png
    ├── sign-up.png
    ├── sign-in.png
    ├── auth-failure.png
    ├── banking-menu.png
    ├── deposit.png
    ├── withdraw.png
    ├── insufficient-balance.png
    ├── check-balance.png
    ├── transfer.png
    ├── receipt.png
    ├── update-account.png
    ├── delete-account.png
    └── exit-confirmation.png
```

## Error Handling

### Comprehensive Validation

| Error Scenario | System Response |
|----------------|-----------------|
| **Account doesn't exist** | "ACCOUNT NUMBER DOESN'T EXIST" - Return to login |
| **Wrong PIN** | "WRONG PIN REMAINING ATTEMPTS: X" - Allow retry |
| **Too many attempts** | "TOO MANY ATTEMPTS! EXITING!!!" - Automatic logout |
| **Insufficient balance** | "INSUFFICIENT BALANCE" - Show current balance, retry option |
| **Invalid account (transfer)** | "ID NUMBER DOESN'T EXISTS" - Allow retry |
| **Invalid PIN format** | "INVALID. PIN MUST BE 4 DIGITS" - Re-prompt |
| **Account not found (update)** | "ACCOUNT DOES NOT EXISTS" - Return to menu |
| **Zero amount entered** | Prompt to continue or return to menu |

### Graceful Exit Options
- Every operation offers exit option (enter 0)
- Confirmation prompts for critical actions
- Option to continue or logout after transactions

## Database Operations

### CRUD Functionality

- **CREATE**: Sign Up creates new account records
- **READ**: All operations read account data
- **UPDATE**: Modify account details and balances
- **DELETE**: Permanently remove accounts

### File Operations
```cobol
OPEN I-O INFO          → Open file for input/output
READ INFO              → Retrieve account record
WRITE INFO-RECORD      → Create new account
REWRITE INFO-RECORD    → Update existing record
DELETE INFO            → Remove account record
CLOSE INFO             → Close file safely
```

## Troubleshooting

### Common Issues

**Issue**: "File not found" error  
**Solution**: Create the directory path specified in line 12 of `atm.cbl` or modify the path to match your system

**Issue**: Display formatting problems  
**Solution**: Ensure terminal is at least 120×30 characters and supports COBOL screen positioning

**Issue**: Colors not showing  
**Solution**: Enable color support in your terminal. Program will still function without colors

**Issue**: "Cannot open file" error  
**Solution**: Check file permissions and ensure the data directory is writable

**Issue**: Account numbers not generating  
**Solution**: System clock issue - verify system time is set correctly

## Development Team

Created by **Johnas-Bautista** and team as an educational project demonstrating:
- Legacy system programming in COBOL
- File handling and indexed sequential access
- Security implementation in banking systems
- Transaction processing logic
- Console-based user interface design

## Contributing

This is an educational project. Contributions for improvements are welcome:

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/improvement`)
3. Commit your changes (`git commit -am 'Add new feature'`)
4. Push to the branch (`git push origin feature/improvement`)
5. Open a Pull Request

## License

This project is open-source and available for educational purposes. Feel free to use, modify, and learn from the code.

## Acknowledgments

- Built with **OpenCOBOL/GnuCOBOL**
- Inspired by real-world ATM systems
- Educational project for learning COBOL programming

---

## 📸 Capturing Screenshots for This README

To complete this documentation, capture screenshots of:

1. **Landing page** with Sign In/Sign Up options
2. **Sign Up screen** showing PIN and account creation
3. **Sign In screen** with account number and PIN entry
4. **Failed authentication** after 3 attempts
5. **Main banking menu** with all 6 operations + exit
6. **Deposit screen** with amount entry
7. **Withdraw screen** with confirmation
8. **Insufficient balance error** message
9. **Balance check** display
10. **Transfer screen** with recipient account
11. **Digital receipt** with timestamp
12. **Update account** screen
13. **Delete account** confirmation
14. **Exit confirmation** prompt

Save screenshots in a `screenshots/` folder in your repository with the names specified in this README.

---

**⚠️ Disclaimer**: This system is for educational purposes only. Do not use in production environments without implementing additional security measures, encryption, and comprehensive testing.

**🎓 Learning Objectives Achieved**:
- ✅ File handling in COBOL
- ✅ Indexed sequential file access
- ✅ Authentication and security
- ✅ Transaction processing
- ✅ Error handling and validation
- ✅ Console interface design
